
// Copyright (c) 2017 Massachusetts Institute of Technology
//
//-
// RVFI_DII + CHERI modifications:
//     Copyright (c) 2020 Alexandre Joannou
//     All rights reserved.
//
//     This software was developed by SRI International and the University of
//     Cambridge Computer Laboratory (Department of Computer Science and
//     Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
//     DARPA SSITH research programme.
//
//     This work was supported by NCSC programme grant 4212611/RFA 15971 ("SafeBet").
//-
//
// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use, copy,
// modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
// BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

`include "ProcConfig.bsv"
import Types::*;
import ProcTypes::*;
import Vector::*;
import FIFOF::*;
import FShow::*;
import Assert::*;
import Ehr::*;
import CacheUtils::*;

// store buffer data block byte size == cache line
typedef TMul#(8, CLineDataNumBytes) SBBlockSz;
typedef Bit#(SBBlockSz) SBBlock;
typedef Vector#(CLineDataNumBytes, Bool) SBByteEn;

// aligned addr
typedef TSub#(AddrSz, LogCLineDataNumBytes) SBBlockAddrSz;
typedef Bit#(SBBlockAddrSz) SBBlockAddr;
function SBBlockAddr getSBBlockAddr(Addr a);
    return truncateLSB(a);
endfunction

// SB block vs. normal data
typedef TDiv#(SBBlockSz, MemDataSz) SBBlockNumMemData;
typedef Bit#(TLog#(SBBlockNumMemData)) SBBlockMemDataSel;
function SBBlockMemDataSel getSBBlockMemDataSel(Addr a);
    return truncate(a >> valueOf(TLog#(MemDataBytes)));
endfunction

// store buffer entry
typedef struct {
    SBBlockAddr addr;
    SBByteEn byteEn;
    CLine line;
    PCHash pcHash;
    Addr boundsOffset;
    Addr boundsVirtBase;
    Addr boundsLength;
} SBEntry deriving(Bits, Eq, FShow);

// result of searching (e.g. load byass)
typedef struct {
    Maybe#(SBIndex) matchIdx;
    Maybe#(MemTaggedData) forwardData; // XXX data is not shifted to match load addr offset
} SBSearchRes deriving(Bits, Eq, FShow);

interface StoreBuffer;
    method Bool isEmpty;
    method Maybe#(SBIndex) getEnqIndex(Addr paddr);
    method Action enq(SBIndex idx, Addr paddr, MemDataByteEn be, MemTaggedData data, PCHash pcHash, 
        Addr boundsOffset, Addr boundsWidth, Addr boundsVirtBase);
    method ActionValue#(SBEntry) deq(SBIndex idx);
    method ActionValue#(Tuple2#(SBIndex, SBEntry)) issue;
    method SBSearchRes search(Addr paddr, ByteOrTagEn be); // load bypass/stall or atomic inst stall
    // check no matching entry for AMO/Lr/Sc issue
    // XXX assume BE has been shifted approriately for paddr offset
    // (for load we need to do that before calling the methods)
    method Bool noMatchLdQ(Addr paddr, ByteOrTagEn be);
    method Bool noMatchStQ(Addr paddr, ByteOrTagEn be);
endinterface

/////////////////
// EHR version //
/////////////////
// method ordering: (empty CF issue CF search) < deq < enq
// This is mostly derived from SplitLSQ ordering of issueLd, deqSt,
// wakupLdStalledBySB. LSQ issueLd is called with SB search, LSQ deqSt is
// called with SB enq, and wakupLdStalledBy is called with SB deq. Since
// issueLd < (deqSt CF wakeupStalledBySB), we have search < (deq, enq). SB
// issue to memory is unrelated to other rules, so just put it at the
// beginning. Ordering between deq and enq are not important. We put deq < enq
// to cut of bypassing path. empty is used in ROB commit, so EHR port = 0
(* synthesize *)
module mkStoreBufferEhr(StoreBuffer);
    Integer emptyPort = 0;
    Integer issuePort = 0;
    Integer searchPort = 0;
    Integer deqPort = 0; // write
    Integer enqPort = 1; // write

    // entries with valid bit
    Vector#(SBSize, Ehr#(2, SBEntry)) entry <- replicateM(mkEhr(?));
    Vector#(SBSize, Ehr#(2, Bool)) valid <- replicateM(mkEhr(False));

    // FIFO of entries to be issued to memory
    FIFOF#(SBIndex) issueQ <- mkUGSizedFIFOF(valueOf(SBSize));
    // FIFO of empty entries
    FIFOF#(SBIndex) freeQ <- mkUGSizedFIFOF(valueOf(SBSize));
    // XXX I use UG FIFOs here, because some methods only call enq/deq in a branch
    // compiler may do somthing wrong with guard if the FIFOs are guarded
    // freeQ never overflow or underflow, so no explicit check is needed
    // issueQ never overflow, so notEmpty check is needed

    // freeQ needs initialization
    Reg#(Bool) inited <- mkReg(False);
    Reg#(SBIndex) initIdx <- mkReg(0);

    rule initFreeQ(!inited);
        freeQ.enq(initIdx);
        initIdx <= initIdx + 1;
        if(initIdx == fromInteger(valueOf(SBSize) - 1)) begin
            inited <= True;
        end
    endrule

    function Maybe#(Bit#(TLog#(n))) searchIndex(function Bool pred(t x), Vector#(n, t) vec);
        case(findIndex(pred, vec)) matches
            tagged Valid .idx: return Valid (pack(idx));
            default: return Invalid;
        endcase
    endfunction

    function Bool noMatch(Addr paddr, ByteOrTagEn be);
        // input BE has been shifted, just pack it
        Bit#(MemDataBytes) ldBE = pack(be.DataMemAccess);

        // data offset within block
        SBBlockMemDataSel sel = getSBBlockMemDataSel(paddr);

        // helper to extract byteEn from entry
        function Bit#(MemDataBytes) getEntryBE(SBIndex idx);
            Vector#(SBBlockNumMemData, MemDataByteEn) byteEn = unpack(pack(entry[idx][searchPort].byteEn));
            return pack(byteEn[sel]);
        endfunction

        // func to determine whether the load matches a store entry
        function Bool matchEntry(Integer i);
            // entry must be valid, addr should match, byte enable should overlap
            Bool sameAddr = getSBBlockAddr(paddr) == entry[i][searchPort].addr;
            Bool beOverlap = be == TagMemAccess || (ldBE & getEntryBE(fromInteger(i))) != 0;
            return valid[i][searchPort] && sameAddr && beOverlap;
        endfunction

        // find match entry and determine return value
        Vector#(SBSize, Integer) idxVec = genVector;
        return !any(matchEntry, idxVec);
    endfunction

    method Bool isEmpty;
        function Bool entryEmpty(Integer i);
            return !valid[i][emptyPort];
        endfunction
        Vector#(SBSize, Integer) idxVec = genVector;
        return all(entryEmpty, idxVec);
    endmethod

    method Maybe#(SBIndex) getEnqIndex(Addr paddr);
        Vector#(SBSize, Integer) idxVec = genVector;
        // first find existing matching entry
        function Bool matchEntry(Integer i);
            return valid[i][enqPort] && entry[i][enqPort].addr == getSBBlockAddr(paddr);
        endfunction
        Maybe#(SBIndex) matchIdx = searchIndex(matchEntry, idxVec);
        if(isValid(matchIdx)) begin
            return matchIdx;
        end
        else begin
            // if no existing entry match, then find an empty entry
            return freeQ.notEmpty ? Valid (freeQ.first) : Invalid;
        end
    endmethod

    method Action enq(SBIndex idx, Addr paddr, MemDataByteEn be, MemTaggedData d, PCHash pcHash, 
        Addr boundsOffset, Addr boundsLength, Addr boundsVirtBase) if(inited);

        // get data offset
        SBBlockMemDataSel sel = getSBBlockMemDataSel(paddr);
        // check whether the entry already exists
        if(valid[idx][enqPort]) begin
            // existing entry: merge
            doAssert(getSBBlockAddr(paddr) == entry[idx][enqPort].addr, "SB enq to existing entry addr should match");
            // update data
            CLine block = entry[idx][enqPort].line;
            block.data[sel] = mergeDataBE(block.data[sel], d.data, be);
            // update tag
            if (pack(be) == ~0) block.tag[sel] = d.tag;
            else if (pack(be) != 0) block.tag[sel] = False;
            // update byte enable
            Vector#(SBBlockNumMemData, MemDataByteEn) byteEn = unpack(pack(entry[idx][enqPort].byteEn));
            byteEn[sel] = unpack(pack(byteEn[sel]) | pack(be));
            // update entry
            entry[idx][enqPort] <= SBEntry {
                addr: getSBBlockAddr(paddr),
                byteEn: unpack(pack(byteEn)),
                line: block,
                pcHash: pcHash,
                boundsOffset: boundsOffset,
                boundsLength: boundsLength,
                boundsVirtBase: boundsVirtBase
            };
            // this entry must have been sent to issueQ
        end
        else begin
            // new entry: set valid
            valid[idx][enqPort] <= True;
            // setup entry
            CLine block = ?;
            block.data[sel] = d.data;
            if (pack(be) == ~0) block.tag[sel] = d.tag;
            else if (pack(be) != 0) block.tag[sel] = False;
            Vector#(SBBlockNumMemData, MemDataByteEn) byteEn = replicate(replicate(False));
            byteEn[sel] = be;
            entry[idx][enqPort] <= SBEntry {
                addr: getSBBlockAddr(paddr),
                byteEn: unpack(pack(byteEn)),
                line: block,
                pcHash: pcHash,
                boundsOffset: boundsOffset,
                boundsLength: boundsLength,
                boundsVirtBase: boundsVirtBase
            };
            // send this entry to issueQ
            doAssert(issueQ.notFull, "SB issueQ should not be full");
            issueQ.enq(idx);
            // remove from freeQ
            doAssert(freeQ.notEmpty, "SB freeQ should not be empty");
            freeQ.deq;
        end
    endmethod

    method ActionValue#(SBEntry) deq(SBIndex idx) if(inited);
        doAssert(valid[idx][deqPort], "SB deq entry must be valid");
        valid[idx][deqPort] <= False; // set entry to invalid
        freeQ.enq(idx); // recycle entry
        return entry[idx][deqPort];
    endmethod

    method ActionValue#(Tuple2#(SBIndex, SBEntry)) issue if(issueQ.notEmpty);
        issueQ.deq;
        SBIndex idx = issueQ.first;
        return tuple2(idx, entry[idx][issuePort]);
    endmethod

    method SBSearchRes search(Addr paddr, ByteOrTagEn be);
        // input BE has been shifted, just pack it
        Bit#(MemDataBytes) ldBE = pack(be.DataMemAccess);

        // data offset within block
        SBBlockMemDataSel sel = getSBBlockMemDataSel(paddr);

        // helper to extract byteEn from entry
        function Bit#(MemDataBytes) getEntryBE(SBIndex idx);
            Vector#(SBBlockNumMemData, MemDataByteEn) byteEn = unpack(pack(entry[idx][searchPort].byteEn));
            return pack(byteEn[sel]);
        endfunction

        // func to determine whether the load matches a store entry
        function Bool matchEntry(Integer i);
            // entry must be valid, addr should match, byte enable should overlap
            Bool sameAddr = getSBBlockAddr(paddr) == entry[i][searchPort].addr;
            Bool beOverlap = be == TagMemAccess || (ldBE & getEntryBE(fromInteger(i))) != 0;
            return valid[i][searchPort] && sameAddr && beOverlap;
        endfunction

        // find match entry and determine return value
        Vector#(SBSize, Integer) idxVec = genVector;
        if(searchIndex(matchEntry, idxVec) matches tagged Valid .idx) begin
            // check whether bytes reading are all covered by the entry
            if(be != TagMemAccess && (getEntryBE(idx) & ldBE) == ldBE) begin
                // fully covered, forward data
                CLine block = entry[idx][searchPort].line;
                return SBSearchRes {
                    matchIdx: Valid (idx),
                    forwardData: Valid (MemTaggedData { tag:  block.tag[sel]
                                                      , data: block.data[sel]})
                };
            end
            else begin
                // interact byte, not fully covered, stall the load
                return SBSearchRes {
                    matchIdx: Valid (idx),
                    forwardData: Invalid
                };
            end
        end
        else begin
            // load should go to memory
            return SBSearchRes {
                matchIdx: Invalid,
                forwardData: Invalid
            };
        end
    endmethod

    method noMatchLdQ = noMatch;
    method noMatchStQ = noMatch;
endmodule

(* synthesize *)
module mkDummyStoreBuffer(StoreBuffer);
    method Bool isEmpty = True;
    method Maybe#(SBIndex) getEnqIndex(Addr paddr) = Invalid;
    method Action enq(SBIndex idx, Addr paddr, MemDataByteEn be, MemTaggedData data, PCHash pcHash, Addr boundsOffset, Addr boundsWidth, Addr boundsVirtBase);
        doAssert(False, "enq should never be called)");
    endmethod
    method ActionValue#(SBEntry) deq(SBIndex idx);
        doAssert(False, "deq should never be called)");
        return ?;
    endmethod
    method ActionValue#(Tuple2#(SBIndex, SBEntry)) issue;
        doAssert(False, "issue should never be called)");
        return ?;
    endmethod
    method SBSearchRes search(Addr paddr, ByteOrTagEn be);
        return SBSearchRes {matchIdx: Invalid, forwardData: Invalid};
    endmethod
    method Bool noMatchLdQ(Addr paddr, ByteOrTagEn be) = True;
    method Bool noMatchStQ(Addr paddr, ByteOrTagEn be) = True;
endmodule
