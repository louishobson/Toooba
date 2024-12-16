
// Copyright (c) 2017 Massachusetts Institute of Technology
//
//-
// RVFI_DII + CHERI modifications:
//     Copyright (c) 2020 Jonathan Woodruff
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
import ClientServer::*;
import DefaultValue::*;
import GetPut::*;
import Types::*;
import ProcTypes::*;
import TlbTypes::*;
import Performance::*;
import FullAssocTlb::*;
import ConfigReg::*;
import Fifos::*;
import FIFO::*;
import CHERICC_Fat::*;
import Cntrs::*;
import SafeCounter::*;
import CacheUtils::*;
import LatencyTimer::*;
import HasSpecBits::*;
import Vector::*;
import Ehr::*;
`ifdef PERFORMANCE_MONITORING
import PerformanceMonitor::*;
import CCTypes::*;
import BlueUtils::*;
import StatCounters::*;
`endif

export DTlbReq(..);
export DTlbResp(..);
export DTlbRqToP(..);
export DTlbToPrefetcher(..);
export DTlbTransRsFromP(..);
export DTlbToParent(..);
export DTlb(..);
export mkDTlb;

typedef `L1_TLB_SIZE DTlbSize;

// req & resp with core
// D TLB also keeps the information of the requesting inst, so we don't need
// extra bookkeeping outside D TLB.
typedef struct {
    instT inst;
    SpecBits specBits;
} DTlbReq#(type instT) deriving(Bits, Eq, FShow);

typedef struct {
    TlbResp resp;
    instT inst;
    SpecBits specBits;
} DTlbResp#(type instT) deriving(Bits, Eq, FShow);

// req & resp with L2 TLB
typedef struct {
    Vpn vpn;
    DTlbReqIdx id;
} DTlbRqToP deriving(Bits, Eq, FShow);

typedef struct {
    // may get page fault: i.e. hit invalid page or
    // get non-leaf page at last-level page table
    Maybe#(TlbEntry) entry;
    DTlbReqIdx id;
} DTlbTransRsFromP deriving(Bits, Eq, FShow);

interface DTlbToParent;
    interface FifoDeq#(DTlbRqToP) rqToP;
    interface FifoEnq#(DTlbTransRsFromP) ldTransRsFromP;
    // after DTLB flush itself, it notifies L2, and wait L2 to flush
    interface Client#(void, void) flush;
endinterface

interface DTlb#(type instT);
    // system consistency related
    method Bool flush_done;
    method Action flush;
    method Action updateVMInfo(VMInfo vm);
    method Bool noPendingReq;

    // req/resp with core
    method Action procReq(DTlbReq#(instT) req);
    method DTlbResp#(instT) procResp;
    method Action deqProcResp;

    interface DTlbToPrefetcher toPrefetcher;

    // req/resp with L2 TLB
    interface DTlbToParent toParent;

    // speculation
    interface SpeculationUpdate specUpdate;

    // performance
    interface Perf#(L1TlbPerfType) perf;
`ifdef PERFORMANCE_MONITORING
    method EventsL1D events;
`endif
endinterface

typedef FullAssocTlb#(DTlbSize) DTlbArray;
module mkDTlbArray(DTlbArray);
    let m <- mkFullAssocTlb(True); // randomness in replacement
    return m;
endmodule

// a pending tlb req may be in following states
typedef union tagged {
    void None;
    void WaitParent;
    DTlbReqIdx WaitPeer;
} DTlbWait deriving(Bits, Eq, FShow);

module mkDTlb#(
    function TlbReq getTlbReq(instT inst),
    function DTlbReq#(instT) createReqForPrefetch(CapPipe vaddr),
    function CapPipe getCap(instT inst))
    (DTlb::DTlb#(instT)) provisos(Bits#(instT, a__), FShow#(instT));
    Bool verbose = True;

    // TLB array
    DTlbArray tlb <- mkDTlbArray;

    // processor init flushing by setting this flag
    Reg#(Bool) needFlush <- mkReg(False);
    // after flushing ITLB itself, we want parent TLB to flush
    Reg#(Bool) waitFlushP <- mkReg(False);

    // current processor VM information
    Reg#(VMInfo) vm_info <- mkReg(defaultValue);

    // pending reqs
    // pendWait should be meaningful even when entry is invalid. pendWait =
    // WaitParent True means this entry is waiting for parent TLB resp;
    // pendWait = WaitPeer means this entry is waiting for a resp initiated by
    // another req. Thus, pendWait must be None when entry is invalid.
    Vector#(DTlbReqNum, Ehr#(2, Bool)) pendValid <- replicateM(mkEhr(False));
    Vector#(DTlbReqNum, Reg#(DTlbWait)) pendWait <- replicateM(mkReg(None));
    Vector#(DTlbReqNum, Reg#(Bool)) pendPoisoned <- replicateM(mkRegU);
    Vector#(DTlbReqNum, Reg#(instT)) pendInst <- replicateM(mkRegU);
    Vector#(DTlbReqNum, Reg#(TlbResp)) pendResp <- replicateM(mkRegU);
    Vector#(DTlbReqNum, Ehr#(2, SpecBits)) pendSpecBits <- replicateM(mkEhr(?));
    Vector#(DTlbReqNum, Reg#(Bool)) pendIsPrefetch <- replicateM(mkRegU);

    // ordering of methods/rules that access pend reqs
    // procReq mutually exclusive with doPRs (no procReq when pRs ready)
    // procResp < {doPRs, procReq}
    // wrongSpec C {procReq, doPRs, procResp}
    // correctSpec C wrongSpec
    // correctSpec CF doPRs
    // {procReq, procResp} < correctSpec (correctSpec is always at end)

    RWire#(void) wrongSpec_procResp_conflict <- mkRWire;
    RWire#(void) wrongSpec_doPRs_conflict <- mkRWire;
    RWire#(void) wrongSpec_handleReq_conflict <- mkRWire;
    RWire#(void) wrongSpec_procReq_conflict <- mkRWire;
    RWire#(void) wrongSpec_prefetcherReq_conflict <- mkRWire;
    RWire#(void) doingWrongSpec <- mkRWire;

    RWire#(void) updatingVMInfo <- mkRWire;

    Reg#(Bit#(4)) prefetchTimeout <- mkReg(0);

    let pendValid_noMiss = getVEhrPort(pendValid, 0);
    let pendValid_wrongSpec = getVEhrPort(pendValid, 0);
    let pendValid_procResp = getVEhrPort(pendValid, 0); // write
    let pendValid_doPRs = getVEhrPort(pendValid, 1); // assert
    let pendValid_procReq = getVEhrPort(pendValid, 1); // write

    let pendSpecBits_wrongSpec = getVEhrPort(pendSpecBits, 0);
    let pendSpecBits_procResp = getVEhrPort(pendSpecBits, 0);
    let pendSpecBits_procReq = getVEhrPort(pendSpecBits, 0); // write
    let pendSpecBits_correctSpec = getVEhrPort(pendSpecBits, 1);

    // free list of pend entries, to cut off path from procResp to procReq
    Fifo#(DTlbReqNum, DTlbReqIdx) freeQ <- mkCFFifo;
    ConfigReg#(Bit#(64)) freeQEnqs <- mkConfigReg(0);
    ConfigReg#(Bit#(64)) freeQDeqs <- mkConfigReg(0);
    Reg#(Bool) freeQInited <- mkReg(False);
    Reg#(DTlbReqIdx) freeQInitIdx <- mkReg(0);

    // req & resp with parent TLB
    Fifo#(DTlbReqNum, DTlbRqToP) rqToPQ <- mkCFFifo; // large enough so won't block on enq
    Fifo#(2, DTlbTransRsFromP) ldTransRsFromPQ <- mkCFFifo;
    // When a resp comes, we first process for the initiating req, then process
    // other reqs that in WaitPeer.
    Reg#(Maybe#(DTlbReqIdx)) respForOtherReq <- mkReg(Invalid);
    // flush req/resp with parent TLB
    Fifo#(1, void) flushRqToPQ <- mkCFFifo;
    Fifo#(1, void) flushRsFromPQ <- mkCFFifo;
    RWire#(DTlbReq#(instT)) rqFromProc <- mkRWire;
    RWire#(DTlbReq#(instT)) rqFromPrefetcher <- mkRWire;

    // perf counters
    LatencyTimer#(DTlbReqNum, 12) latTimer <- mkLatencyTimer; // max latency: 4K cycles
    Fifo#(1, L1TlbPerfType) perfReqQ <- mkCFFifo;
`ifdef PERF_COUNT
    Fifo#(1, PerfResp#(L1TlbPerfType)) perfRespQ <- mkCFFifo;
    Reg#(Bool) doStats <- mkConfigReg(False);
    Count#(Data) accessCnt <- mkCount(0);
    Count#(Data) prefetchAccessCnt <- mkCount(0);
    Count#(Data) missParentCnt <- mkCount(0);
    Count#(Data) prefetchMissParentCnt <- mkCount(0);
    Count#(Data) missParentLat <- mkCount(0);
    Count#(Data) prefetchMissParentLat <- mkCount(0);
    Count#(Data) missPeerCnt <- mkCount(0);
    Count#(Data) missPeerLat <- mkCount(0);
    Count#(Data) hitUnderMissCnt <- mkCount(0);
    Count#(Data) allMissCycles <- mkCount(0);

    rule doPerf;
        let t <- toGet(perfReqQ).get;
        Data d = (case(t)
            L1TlbAccessCnt: (accessCnt);
            L1TlbMissParentCnt: (missParentCnt);
            L1TlbMissParentLat: (missParentLat);
            L1TlbMissPeerCnt: (missPeerCnt);
            L1TlbMissPeerLat: (missPeerLat);
            L1TlbHitUnderMissCnt: (hitUnderMissCnt);
            L1TlbAllMissCycles: (allMissCycles);
            default: (0);
        endcase);
        perfRespQ.enq(PerfResp {
            pType: t,
            data: d
        });
    endrule

    rule incrAllMissCycles(doStats);
        function Bool isMiss(DTlbWait x) = x != None;
        when(all(isMiss, readVReg(pendWait)), allMissCycles.incr(1));
    endrule
`endif
`ifdef PERFORMANCE_MONITORING
    Array #(Reg #(EventsL1D)) perf_events <- mkDRegOR (3, unpack (0));
`endif

    // do flush: start when all misses resolve
    Bool noMiss = all(\== (False) , readVReg(pendValid_noMiss));

    rule doStartFlush(needFlush && !waitFlushP && noMiss);
        tlb.flush;
        // request parent TLB to flush
        flushRqToPQ.enq(?);
        waitFlushP <= True;
        if(verbose) $display("[DTLB] flush begin");
`ifdef PERFORMANCE_MONITORING
        EventsL1D ev = unpack(0);
        //ev.evt_TLB_FLUSH = 1;
        perf_events[2] <= ev;
`endif
    endrule

    rule doFinishFlush(needFlush && waitFlushP);
        flushRsFromPQ.deq;
        needFlush <= False;
        waitFlushP <= False;
        if(verbose) $display("[DTLB] flush done");
    endrule

    // get resp from parent TLB
    // At high level, this rule is always exclusive with doStartFlush, though
    // we don't bother to make compiler understand this...
    rule doPRs(ldTransRsFromPQ.notEmpty);
        let pRs = ldTransRsFromPQ.first;
        // the current req being served is either the initiating req or other
        // req pending on the same resp
        let idx = fromMaybe(pRs.id, respForOtherReq);
        TlbReq r = getTlbReq(pendInst[idx]);
        $display("%t DTlb doPRs", $time);

        if(pendPoisoned[idx]) begin
            // poisoned inst, do nothing
            if(verbose) $display("[DTLB] refill poisoned: idx %d; ", idx, fshow(r));
        end
        else if(pRs.entry matches tagged Valid .en) begin
            // check permission
            if (verbose)
                $display("dPRs: vm_info: ", fshow(vm_info),
                         "      en     : ", fshow(en),
                         "      r      : ", fshow(r)
                         );
            let permCheck = hasVMPermission(vm_info,
                                            en.pteType,
                                            en.pteUpperType,
                                            en.ppn,
                                            en.level,
                                            r.write ? DataStore : DataLoad,
                                            r.capStore,
                                            r.potentialCapLoad);
            if (permCheck.allowed) begin
                // fill TLB, and record resp
                if (!pendIsPrefetch[idx]) begin
                tlb.addEntry(en);
                end
                let trans_addr = translate(r.addr, en.ppn, en.level);
                pendResp[idx] <= tuple3(trans_addr, Invalid, permCheck.allowCap);
                if(verbose) begin
                    $display("[DTLB] refill: idx %d; ", idx, fshow(r),
                             "; ", fshow(trans_addr));
                end
            end
            else begin
                // page fault
                Exception fault = permCheck.excCode;
                pendResp[idx] <= tuple3(?, Valid (fault), False);
                if(verbose) begin
                    $display("[DTLB] refill no permission: idx %d; ", idx, fshow(r));
                end
            end
        end
        else begin
            // page fault
            Exception fault = r.write ? excStorePageFault : excLoadPageFault;
            pendResp[idx] <= tuple3(?, Valid (fault), False);
            if(verbose) $display("[DTLB] refill page fault: idx %d; ", idx, fshow(r));
        end

        // get parent resp, miss resolved, reset wait bit
        pendWait[idx] <= None;

        doAssert(pendValid_doPRs[idx], "entry must be valid");
        if(isValid(respForOtherReq)) begin
            doAssert(pendWait[idx] == WaitPeer (pRs.id), "entry must be waiting for resp");
        end
        else begin
            doAssert(pendWait[idx] == WaitParent, "entry must be waiting for resp");
        end

        // find another req waiting for this resp
        function Bool waitForResp(DTlbReqIdx i);
            // we can ignore pendValid here, because not-None pendWait implies
            // pendValid is true
            return pendWait[i] == WaitPeer (pRs.id) && i != idx;
        endfunction
        Vector#(DTlbReqNum, DTlbReqIdx) idxVec = genWith(fromInteger);
        if(find(waitForResp, idxVec) matches tagged Valid .i) begin
            // still have req waiting for this resp, keep processing
            respForOtherReq <= Valid (i);
            doAssert(pendValid_doPRs[i], "waiting entry must be valid");
        end
        else begin
            // all req done, deq the pRs
            respForOtherReq <= Invalid;
            ldTransRsFromPQ.deq;
        end

        // perf: miss
        let lat <- latTimer.done(idx);
`ifdef PERF_COUNT
        if(doStats) begin
            if(isValid(respForOtherReq)) begin
                missPeerLat.incr(zeroExtend(lat));
                missPeerCnt.incr(1);
            end
            else begin
                if (!pendIsPrefetch[idx]) begin
                    missParentLat.incr(zeroExtend(lat));
                    missParentCnt.incr(1);
                end
                else begin
                    prefetchMissParentLat.incr(zeroExtend(lat));
                    prefetchMissParentCnt.incr(1);
                end
            end
        end
`endif
`ifdef PERFORMANCE_MONITORING
        EventsL1D ev = unpack(0);
        if (!pendIsPrefetch[idx]) begin
            ev.evt_TLB_MISS_LAT = saturating_truncate(lat);
            ev.evt_TLB_MISS = 1;
        end
        else begin
            ev.evt_AMO_MISS = 1;
        end
        perf_events[0] <= ev;
`endif
        // conflict with wrong spec
        wrongSpec_doPRs_conflict.wset(?);
    endrule

    // init freeQ
    rule doInitFreeQ(!freeQInited);
        freeQ.enq(freeQInitIdx);
        freeQEnqs <= freeQEnqs + 1;
        freeQInitIdx <= freeQInitIdx + 1;
        if(freeQInitIdx == fromInteger(valueof(DTlbReqNum) - 1)) begin
            freeQInited <= True;
        end
    endrule

    // idx of entries that are ready to resp to proc
    function Maybe#(DTlbReqIdx) validProcRespIdx;
        function Bool validResp(DTlbReqIdx i);
            return pendValid_procResp[i] && pendWait[i] == None && !pendPoisoned[i] && !pendIsPrefetch[i];
        endfunction
        Vector#(DTlbReqNum, DTlbReqIdx) idxVec = genWith(fromInteger);
        return find(validResp, idxVec);
    endfunction

    function Maybe#(DTlbReqIdx) validPrefetcherRespIdx;
        function Bool validResp(DTlbReqIdx i);
            return pendValid_procResp[i] && pendWait[i] == None && !pendPoisoned[i] && pendIsPrefetch[i];
        endfunction
        Vector#(DTlbReqNum, DTlbReqIdx) idxVec = genWith(fromInteger);
        return find(validResp, idxVec);
    endfunction

    function Maybe#(DTlbReqIdx) poisonedProcRespIdx;
        function Bool poisonedResp(DTlbReqIdx i);
            return pendValid_procResp[i] && pendWait[i] == None && pendPoisoned[i];
        endfunction
        Vector#(DTlbReqNum, DTlbReqIdx) idxVec = genWith(fromInteger);
        return find(poisonedResp, idxVec);
    endfunction

    // drop poisoned resp
    rule doPoisonedProcResp(poisonedProcRespIdx matches tagged Valid .idx &&& freeQInited);
        $display ("%t Dtlb dropPoisoned", $time);
        pendValid_procResp[idx] <= False;
        freeQ.enq(idx);
        freeQEnqs <= freeQEnqs + 1;
        // conflict with wrong spec
        wrongSpec_procResp_conflict.wset(?);
    endrule

    rule printStatus;
        /*
        $display ("%t D Tlb ldTransRsFromPq empty %b full %b rqtopq empty %b full %b freeq empty %b full %b candoprocreq %b nomiss %b", $time, 
        !ldTransRsFromPQ.notEmpty, !ldTransRsFromPQ.notFull, !rqToPQ.notEmpty, !rqToPQ.notFull,
        !freeQ.notEmpty, !freeQ.notFull, 
        (!needFlush && !ldTransRsFromPQ.notEmpty && rqToPQ.notFull && freeQInited && freeQ.notEmpty),
        noMiss, fshow(respForOtherReq)
        */
        //$display("L1 DTlb accessCnt %d prefetchAccessCnt %d missParentCnt %d prefetchMissParentCnt %d missParentLat %d prfMissParentLat %d missPeerCnt %d missPeerLat %d hitUnderMissCnt %d totalMissCycles %d freeqlen",
            //accessCnt, prefetchAccessCnt, missParentCnt, prefetchMissParentCnt, missParentLat, prefetchMissParentLat, missPeerCnt, missPeerLat, hitUnderMissCnt, allMissCycles, freeQEnqs-freeQDeqs);
    endrule

    rule handleMergedRq if (!needFlush && !ldTransRsFromPQ.notEmpty && rqToPQ.notFull && freeQInited &&
        (isValid(rqFromProc.wget) || isValid(rqFromPrefetcher.wget))
    ); 
        $display ("%t DTlb handleMergedRq", $time);
        Bool isPrefetch = False;
        DTlbReq#(instT) req = unpack(0);
        if (rqFromProc.wget matches tagged Valid .t) begin
            req = t;
            isPrefetch = False;
        end
        else if (rqFromPrefetcher.wget matches tagged Valid .t) begin
            req = t;
            isPrefetch = True;
        end
        else begin
            doAssert(False, "Unreachable");
        end
        // allocate MSHR entry
        freeQ.deq;
        freeQDeqs <= freeQDeqs + 1;
        DTlbReqIdx idx = freeQ.first;
        doAssert(!pendValid_procReq[idx], "free entry cannot be valid");
        doAssert(pendWait[idx] == None, "entry cannot wait for parent resp");

        pendValid_procReq[idx] <= True;
        pendPoisoned[idx] <= False;
        pendInst[idx] <= req.inst;
        pendIsPrefetch[idx] <= isPrefetch;
        pendSpecBits_procReq[idx] <= req.specBits;
        // pendWait and pendResp are set later in this method

        // try to translate
        TlbReq r = getTlbReq(req.inst);

`ifdef SECURITY
        // Security check
        // Forbid any data load shared outside of the protection domain
        // if shared load are not allowed
        // No need to special case M mode with special vm_info value because we
        // assume that we allow shared load all the time when in M mode.
        // (Because we are always non speculative in M mode)
        if (!vm_info.sanctum_authShared && outOfProtectionDomain(vm_info, r.addr))begin
            pendWait[idx] <= None;
            pendResp[idx] <= tuple3(?, Valid (excLoadAccessFault), False);
        end
`else
        // No security check
        if (False) begin
            noAction;
        end
`endif
        else if (vm_info.sv39) begin
            let vpn = getVpn(r.addr);
            let trans_result = tlb.translate(vpn, vm_info.asid);
            if (!validVirtualAddress(r.addr)) begin
                // page fault
                Exception fault = r.write ? excStorePageFault : excLoadPageFault;
                pendWait[idx] <= None;
                pendResp[idx] <= tuple3(?, Valid (fault), False);
                if(verbose) $display("[DTLB] req invalid virtual address: idx %d; ", idx, fshow(r));
            end else if (trans_result.hit) begin
                // TLB hit
                let entry = trans_result.entry;
                // check permission
            $display("procReq: vm_info: ", fshow(vm_info),
                     "         en     : ", fshow(entry),
                     "         r      : ", fshow(r)
                     );
                let permCheck = hasVMPermission(vm_info,
                                                entry.pteType,
                                                entry.pteUpperType,
                                                entry.ppn,
                                                entry.level,
                                                r.write ? DataStore : DataLoad,
                                                r.capStore,
                                                r.potentialCapLoad);
                $display("Permission check output 2: ", fshow(permCheck));
                if (permCheck.allowed) begin
                    // update TLB replacement info
                    tlb.updateRepByHit(trans_result.index);
                    // translate addr
                    /*
                    `ifdef PERFORMANCE_MONITORING
                            EventsL1D ev = unpack(0);
                            ev.evt_TLB = 1;
                            if (entry.level == 0) ev.evt_TLB_MISS = 1;
                            if (entry.level == 1) ev.evt_TLB_MISS_LAT = 1;
                            if (entry.level == 2) ev.evt_TLB_FLUSH = 1;
                            perf_events[1] <= ev;
                    `endif
                    */
                    
                    Addr trans_addr = translate(r.addr, entry.ppn, entry.level);
                    pendWait[idx] <= None;
                    pendResp[idx] <= tuple3(trans_addr, Invalid, permCheck.allowCap);
                    if(verbose) begin
                        $display("[DTLB] req (hit): idx %d; ", idx, fshow(r),
                                 "; ", fshow(trans_result));
                    end
`ifdef PERF_COUNT
                    // perf: hit under miss
                    if(doStats && readVReg(pendWait) != replicate(None)) begin
                        hitUnderMissCnt.incr(1);
                    end
`endif
                end
                else begin
                    // page fault
                    Exception fault = permCheck.excCode;
                    pendWait[idx] <= None;
                    pendResp[idx] <= tuple3(?, Valid (fault), False);
                    if(verbose) $display("[DTLB] req no permission: idx %d; ", idx, fshow(r));
                end
            end
            else begin
                // TLB miss, req to parent TLB only if there is no existing req
                // for the same VPN already waiting for parent TLB resp
                function Bool reqSamePage(DTlbReqIdx i);
                    // we can ignore pendValid here, because not-None pendWait implies
                    // pendValid is true
                    let r_i = getTlbReq(pendInst[i]);
                    return pendWait[i] == WaitParent && getVpn(r.addr) == getVpn(r_i.addr);
                endfunction
                Vector#(DTlbReqNum, DTlbReqIdx) idxVec = genWith(fromInteger);
                if(find(reqSamePage, idxVec) matches tagged Valid .i) begin
                    // peer entry has already requested, so don't send duplicate req
                    pendWait[idx] <= WaitPeer (i);
                    doAssert(pendValid_procReq[i], "peer entry must be valid");
                    if(verbose) begin
                        $display("[DTLB] req miss, pend on peer: idx %d, ",
                                 idx, "; ", fshow(r), "; ", fshow(i));
                    end
                end
                else begin
                    // this is the first req for this VPN
                    pendWait[idx] <= WaitParent;
                    rqToPQ.enq(DTlbRqToP {
                        vpn: vpn,
                        id: idx
                    });
                    if(verbose) begin
                        $display("[DTLB] req miss, send to parent: idx %d, ",
                                 idx, fshow(r));
                    end
                end
                // perf: miss
                latTimer.start(idx);
            end
        end
        else begin
            // bare mode
            pendWait[idx] <= None;
            pendResp[idx] <= tuple3(r.addr, Invalid, True);
            if(verbose) $display("DTLB %m req (bare): ", fshow(r));
        end

`ifdef PERF_COUNT
        // perf: access
        if(doStats) begin
            if (!pendIsPrefetch[idx])
                accessCnt.incr(1);
            else 
                prefetchAccessCnt.incr(1);
        end
`endif
 `ifdef PERFORMANCE_MONITORING
         EventsL1D ev = unpack(0);
         //ev.evt_TLB = 1;
         perf_events[1] <= ev;
 `endif
        // conflict with wrong spec
        wrongSpec_handleReq_conflict.wset(?);
    endrule

    rule decrementPrefetchTimeout if (!isValid(doingWrongSpec.wget) && prefetchTimeout > 0);
        $display ("Dtlb dectimeout");
        prefetchTimeout <= prefetchTimeout - 1;
    endrule
    
    method Action flush if(!needFlush);
        needFlush <= True;
        waitFlushP <= False;
        // this won't interrupt current processing, since
        // (1) miss process will continue even if needFlush=True
        // (2) flush truly starts when there is no pending req
    endmethod

    method Bool flush_done = !needFlush;

    method Action updateVMInfo(VMInfo vm);
        $display("%t DTlb updateVMInfo", $time);
        vm_info <= vm;
    endmethod

    // Since this method is called at commit stage to determine no in-flight
    // TLB req, even poisoned req should be considered as pending, because it
    // may be in L2 TLB.
    method Bool noPendingReq = noMiss;

    // We do not accept new req when flushing flag is set. We also do not
    // accept new req when parent resp is ready. This avoids bypass in TLB. We
    // also check rqToPQ not full. This simplifies the guard, i.e., it does not
    // depend on whether we hit in TLB or not.
    method Action procReq(DTlbReq#(instT) req) if(
        !needFlush && !ldTransRsFromPQ.notEmpty && rqToPQ.notFull && freeQInited && freeQ.notEmpty
    );
        $display ("%t DTlb procReq ", $time, fshow(req));
        wrongSpec_procReq_conflict.wset(?);
        rqFromProc.wset(req);
    endmethod

    interface DTlbToPrefetcher toPrefetcher;
        method Action prefetcherReq(CapPipe vaddr) if(
            !isValid(rqFromProc.wget) && !needFlush && !ldTransRsFromPQ.notEmpty && 
            rqToPQ.notFull && freeQInited && freeQ.notEmpty && !isValid(doingWrongSpec.wget) && (prefetchTimeout == 0) && (freeQEnqs-freeQDeqs >= 3)
        );
            DTlbReq#(instT) req = createReqForPrefetch(vaddr);
            //wrongSpec_prefetcherReq_conflict.wset(?);
            $display ("%t DTlb prefetcherReq ", $time, fshow(req));
            rqFromPrefetcher.wset(req);
        endmethod

        method Action deqPrefetcherResp if(
            validPrefetcherRespIdx matches tagged Valid .idx &&& freeQInited &&& !isValid(doingWrongSpec.wget)
        );
            $display ("%t DTlb deqPrefetcherReq ", $time, fshow(idx));
            pendValid_procResp[idx] <= False;
            freeQ.enq(idx);
            freeQEnqs <= freeQEnqs + 1;
            // conflict with wrong spec
            //wrongSpec_procResp_conflict.wset(?);
        endmethod

        method DTlbRespToPrefetcher prefetcherResp if(
            validPrefetcherRespIdx matches tagged Valid .idx &&& freeQInited
        );
            let resp = pendResp[idx];
            return DTlbRespToPrefetcher {
                paddr: tpl_1(resp),
                haveException: isValid(tpl_2(resp)),
                permsCheckPass: tpl_3(resp),
                cap: getCap(pendInst[idx])
            };
        endmethod
    endinterface

    method Action deqProcResp if(
        validProcRespIdx matches tagged Valid .idx &&& freeQInited
    );
        $display ("%t DTlb deqProcReq ", $time, fshow(idx));
        pendValid_procResp[idx] <= False;
        freeQ.enq(idx);
        freeQEnqs <= freeQEnqs + 1;
        // conflict with wrong spec
        wrongSpec_procResp_conflict.wset(?);
    endmethod

    method DTlbResp#(instT) procResp if(
        validProcRespIdx matches tagged Valid .idx &&& freeQInited
    );
        return DTlbResp {
            inst: pendInst[idx],
            resp: pendResp[idx],
            specBits: pendSpecBits_procResp[idx]
        };
    endmethod

    interface DTlbToParent toParent;
        interface rqToP = toFifoDeq(rqToPQ);
        interface ldTransRsFromP = toFifoEnq(ldTransRsFromPQ);
        interface Client flush;
            interface request = toGet(flushRqToPQ);
            interface response = toPut(flushRsFromPQ);
        endinterface
    endinterface

    interface SpeculationUpdate specUpdate;
        method Action incorrectSpeculation(Bool kill_all, SpecTag x);
            // poison entries
            $display ("%t Dtlb incorrectSpeculation killall %b spectag", $time, kill_all, fshow(x));
            if (kill_all) begin
                prefetchTimeout <= 4;
            end
            for(Integer i = 0 ; i < valueOf(DTlbReqNum) ; i = i+1) begin
                if((kill_all || pendSpecBits_wrongSpec[i][x] == 1'b1) || (pendIsPrefetch[i])) begin
                    pendPoisoned[i] <= True;
                end
            end
            // make conflicts with procReq, doPRs, procResp
            wrongSpec_procReq_conflict.wset(?);
            //wrongSpec_prefetcherReq_conflict.wset(?);
            wrongSpec_doPRs_conflict.wset(?);
            wrongSpec_procResp_conflict.wset(?);
            wrongSpec_handleReq_conflict.wset(?);
            doingWrongSpec.wset(?);
        endmethod
        method Action correctSpeculation(SpecBits mask);
            //$display ("%t D tlb correctSpeculation mask ", $time, fshow(mask));
            // clear spec bits for all entries
            for(Integer i = 0 ; i < valueOf(DTlbReqNum) ; i = i+1) begin
                let new_spec_bits = pendSpecBits_correctSpec[i] & mask;
                pendSpecBits_correctSpec[i] <= new_spec_bits;
            end
        endmethod
    endinterface

    interface Perf perf;
        method Action setStatus(Bool stats);
`ifdef PERF_COUNT
            doStats <= stats;
`else
            noAction;
`endif
        endmethod

        method Action req(L1TlbPerfType r);
            perfReqQ.enq(r);
        endmethod

        method ActionValue#(PerfResp#(L1TlbPerfType)) resp;
`ifdef PERF_COUNT
            perfRespQ.deq;
            return perfRespQ.first;
`else
            perfReqQ.deq;
            return PerfResp {
                pType: perfReqQ.first,
                data: 0
            };
`endif
        endmethod

        method Bool respValid;
`ifdef PERF_COUNT
            return perfRespQ.notEmpty;
`else
            return perfReqQ.notEmpty;
`endif
        endmethod
    endinterface
`ifdef PERFORMANCE_MONITORING
    method EventsL1D events = perf_events[0];
`endif
endmodule
