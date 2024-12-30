
// Copyright (c) 2017 Massachusetts Institute of Technology
// Portions Copyright (c) 2019-2020 Bluespec, Inc.
//
//-
// RVFI_DII + CHERI modifications:
//     Copyright (c) 2020 Jessica Clarke
//     Copyright (c) 2020 Alexandre Joannou
//     Copyright (c) 2020 Peter Rugg
//     Copyright (c) 2020 Jonathan Woodruff
//     Copyright (c) 2024 Franz Fuchs
//     All rights reserved.
//
//     This software was developed by SRI International and the University of
//     Cambridge Computer Laboratory (Department of Computer Science and
//     Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
//     DARPA SSITH research programme.
//
//     This work was supported by NCSC programme grant 4212611/RFA 15971 ("SafeBet").
//     This software was developed by the University of  Cambridge
//     Department of Computer Science and Technology under the
//     SIPP (Secure IoT Processor Platform with Remote Attestation)
//     project funded by EPSRC: EP/S030868/1
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
import Vector::*;
import GetPut::*;
import Cntrs::*;
import ConfigReg::*;
import DReg::*;
import FIFO::*;
import FIFOF::*;
import Types::*;
import ProcTypes::*;
import CCTypes::*;
import Performance::*;
import ReorderBuffer::*;
import ReorderBufferSynth::*;
import RenamingTable::*;
import CsrFile::*;
import StoreBuffer::*;
import VerificationPacket::*;
import RenameDebugIF::*;
import CHERICap::*;
import CHERICC_Fat::*;
import ISA_Decls_CHERI::*;
import RegFile::*; // Just for the interface
`ifdef PERFORMANCE_MONITORING
import StatCounters::*;
`endif

`ifdef RVFI
import RVFI_DII_Types::*;
`endif
import Cur_Cycle :: *;

`ifdef INCLUDE_TANDEM_VERIF
import Trace_Data2 :: *;
`endif

`ifdef KONATA
import Ehr :: *;
`endif

typedef struct {
    // info about the inst blocking at ROB head
    Addr pc;
    IType iType;
    Maybe#(Trap) trap;
    RobInstState state;
    Bool claimedPhyReg;
    Bool ldKilled;
    Bool memAccessAtCommit;
    Bool lsqAtCommitNotified;
    Bool nonMMIOStDone;
    Bool epochIncremented;
    SpecBits specBits;
    // info about LSQ/TLB
    Bool stbEmpty;
    Bool stqEmpty;
    Bool tlbNoPendingReq;
    // CSR info: privilege mode
    Bit#(2) prv;
    // inst count
    Data instCount;
} CommitStuck deriving(Bits, Eq, FShow);

interface CommitInput;
    // func units
    interface ReorderBufferSynth robIfc;
    interface RegRenamingTable rtIfc;
    interface CsrFile csrfIfc;
    // no stores
    method Bool stbEmpty;
    method Bool stqEmpty;
    // notify LSQ that inst has reached commit
    interface Vector#(SupSize, Put#(LdStQTag)) lsqSetAtCommit;
    // method for getting translated addresses for tracing.
    interface Vector#(SupSize, RegFile#(LdStQTag, Addr)) lookupPAddr;
    // TLB has stopped processing now
    method Bool tlbNoPendingReq;
    // Pause committing, probably for buffered wrongSpec
    method Bool pauseCommit;
    // set flags
    method Action setFlushTlbs;
    method Action setUpdateVMInfo;
    method Action setFlushReservation;
    method Action setFlushBrPred; // security
    method Action setFlushCaches; // security
    method Action setReconcileI; // recocile I$
    method Action setReconcileD; // recocile D$
    // redirect
    method Action killAll;
    method Action redirectPc(
        CapMem trap_pc,
        SpecBits specBits
`ifdef RVFI_DII
        , Dii_Parcel_Id dii_pid
`endif
    );
    method Action setFetchWaitRedirect;
`ifdef INCLUDE_GDB_CONTROL
    method Action setFetchWaitFlush;
`endif
    method Action incrementEpoch;
    // record if we commit a CSR inst or interrupt
    method Action commitCsrInstOrInterrupt;
    // performance
    method Bool doStats;
    // deadlock check
    method Bool checkDeadlock;

`ifdef PERFORMANCE_MONITORING
`ifdef CONTRACTS_VERIFY
    // update branch targets
    method Action updateTargets(Vector#(SupSize, Maybe#(CapMem)) targets);
    // update return targets
    method Action updateReturnTargets(Vector#(SupSize, Maybe#(CapMem)) returnTargets);
`endif
`endif

`ifdef INCLUDE_TANDEM_VERIF
    interface Vector #(SupSize, Put #(Trace_Data2)) v_to_TV;
`endif
endinterface

typedef struct {
    RenameError err;
    Addr pc;
    IType iType;
    Maybe#(Trap) trap;
    SpecBits specBits;
} RenameErrInfo deriving(Bits, Eq, FShow);

typedef struct {
    CapMem trap_pc;
`ifdef RVFI_DII
    Dii_Parcel_Id dii_pid;
`endif
} RedirectInfo deriving(Bits, Eq, FShow);

interface CommitStage;
    // performance
    method Data getPerf(ComStagePerfType t);
    // deadlock check
    interface Get#(CommitStuck) commitInstStuck;
    interface Get#(CommitStuck) commitUserInstStuck;
    // rename debug
    method Action startRenameDebug;
    interface Get#(RenameErrInfo) renameErr;
`ifdef RVFI
    // RVFI trace report. Not an input?
    method Get#(Rvfi_Traces) rvfi;
`endif
`ifdef INCLUDE_GDB_CONTROL
   method Bool is_debug_halted;
   method Action debug_resume;
`endif
`ifdef PERFORMANCE_MONITORING
   method EventsCore events;
`endif
endinterface

// we apply actions the end of commit rule
// use struct to record actions to be done
typedef struct {
    CapMem pc;
    Addr addr;
    Trap trap;
    Bit #(32) orig_inst;
`ifdef RVFI
    ToReorderBuffer x;
`endif
} CommitTrap deriving(Bits, FShow);

`ifdef RVFI
function Bool is_16b_inst (Bit #(n) inst);
   return (inst [1:0] != 2'b11);
endfunction

typedef struct {
    Data sepc;
    Data mepc;
    Data stvec;
    Data mtvec;
} TraceStateBundle deriving(Bits, FShow);

function Maybe#(RVFI_DII_Execution#(DataSz,DataSz)) genRVFI(ToReorderBuffer rot, Dii_Id traceCnt, TraceStateBundle tsb, Data next_pc, Addr paddr);
    Addr addr = 0;
    Data data = 0;
    Data wdata = 0;
    ByteEn rmask = replicate(False);
    ByteEn wmask = replicate(False);
    Bit#(5) rd = 0;
    if (!isValid(rot.trap)) begin
        if (rot.dst matches tagged Valid .regWrite) begin
            if (regWrite matches tagged Gpr .regNum) begin
                data = rot.traceBundle.regWriteData;
                rd = regNum;
            end
        end
        case (rot.ppc_vaddr_csrData) matches
            tagged VAddr .vaddr: begin
                addr = vaddr;
`ifdef PADDR_RVFI
                addr = paddr;
`endif
                case (rot.lsqTag) matches
                    tagged Ld .l: rmask = rot.traceBundle.memByteEn;
                    tagged St .s: begin
                        wmask = rot.traceBundle.memByteEn;
                        wdata = rot.traceBundle.regWriteData;
                        if (rot.iType == Sc) data = rot.traceBundle.memByteEn[0] ? 0:1;
                    end
                endcase
            end
            tagged PPC .ppc: begin
                CapPipe cp = cast(ppc);
                next_pc = getAddr(cp);
            end
            tagged CSRData .csrdata: if (rot.iType == Csr) data = getAddr(csrdata);
        endcase
    end
    CapPipe pipePc = cast(rot.pc);
    return tagged Valid RVFI_DII_Execution {
        rvfi_order: zeroExtend(pack(traceCnt)),
        rvfi_trap: isValid(rot.trap),
        rvfi_halt: False,
        rvfi_intr: ?,
        rvfi_insn: rot.orig_inst,
        rvfi_rs1_addr: rot.orig_inst[19:15],
        rvfi_rs2_addr: rot.orig_inst[24:20],
        rvfi_rs1_data: ?,
        rvfi_rs2_data: ?,
        rvfi_pc_rdata: getOffset(pipePc),
        rvfi_pc_wdata: next_pc,
        rvfi_mem_wdata: wdata,
        rvfi_rd_addr: rd,
        rvfi_rd_wdata: ((rd==0) ? 0:data),
        rvfi_mem_addr: addr,
        rvfi_mem_rmask: pack(rmask),
        rvfi_mem_wmask: pack(wmask),
        rvfi_mem_rdata: data
    };
endfunction
`endif

`ifdef INCLUDE_GDB_CONTROL

typedef enum {
   RUN_STATE_RUNNING,                // Normal state
   RUN_STATE_DEBUGGER_HALTED         // When halted for debugger
   } Run_State
deriving (Eq, FShow, Bits);

`endif

module mkCommitStage#(CommitInput inIfc)(CommitStage);
    Bool verbose = False;

    Integer verbosity = 0;   // Bluespec: for lightweight verbosity trace

    // Used to inform tandem-verifier about program order.
    // 0 is used to indicate we've just come out of reset
    // TODO: we could use fewer bits and allow and recognize wraparound.
    Reg #(Bit #(64)) rg_serial_num <- mkReg (0);

`ifdef INCLUDE_TANDEM_VERIF
    FIFOF #(ToReorderBuffer) f_rob_data <- mkFIFOF;
`endif

`ifdef INCLUDE_GDB_CONTROL
   Reg #(Run_State) rg_run_state   <- mkReg (RUN_STATE_RUNNING);
`endif

`ifdef INCLUDE_TANDEM_VERIF
   Integer way0 = 0;

   ToReorderBuffer       no_deq_data = ?;
   Bit #(5)              no_fflags   = ?;
   Data                  no_mstatus  = ?;
   Maybe #(Trap_Updates) no_trap_updates = tagged Invalid;
   Maybe #(RET_Updates)  no_ret_updates  = tagged Invalid;

   function Action fa_to_TV (Integer                way,
                             Bit #(64)              serial_num,
                             Maybe #(Tuple2 #(Bit #(12), Data)) maybe_csr_upd,
                             ToReorderBuffer        deq_data,
                             Bit #(5)               fflags,
                             Data                   mstatus,
                             Maybe #(Trap_Updates)  m_trap_updates,
                             Maybe #(RET_Updates)   m_ret_updates);
      action
         let tval     = (m_trap_updates matches tagged Valid .tu ? tu.tval : deq_data.tval);
         let upd_pc   = (m_ret_updates matches tagged Valid .ru ? ru.new_pc : deq_data.pc);
         let x = Trace_Data2 {serial_num:           serial_num,
                              maybe_csr_upd:        maybe_csr_upd,
                              pc:                   upd_pc,
                              orig_inst:            deq_data.orig_inst,
                              iType:                deq_data.iType,
                              dst:                  deq_data.dst,
                              dst_data:             deq_data.dst_data,
                              store_data:           deq_data.store_data,
                              store_data_BE:        deq_data.store_data_BE,
                              csr:                  deq_data.csr,
                              trap:                 deq_data.trap,
                              tval:                 tval,
                              ppc_vaddr_csrData:    deq_data.ppc_vaddr_csrData,
                              fflags:               fflags,    // deq_data.fflags only has incremental flags
                              will_dirty_fpu_state: deq_data.will_dirty_fpu_state,
                              mstatus:              mstatus,    // when SD/XS/FS have changed

                              // Trap and RET updates
                              prv:                  (  m_trap_updates matches tagged Valid .tu
                                                     ? tu.prv
                                                     : (m_ret_updates matches tagged Valid .ru
                                                        ? ru.prv
                                                        : ?)),
                              status:               (  m_trap_updates matches tagged Valid .tu
                                                     ? tu.status
                                                     : (m_ret_updates matches tagged Valid .ru
                                                        ? ru.status
                                                        : ?)),
                              tvec:                 fromMaybe (?, m_trap_updates).new_pc,
                              cause:                fromMaybe (?, m_trap_updates).cause,
                              epc:                  fromMaybe (?, m_trap_updates).epc
                              };
         inIfc.v_to_TV [way].put (x);
      endaction
   endfunction

   Reg #(Bool) rg_just_after_reset <- mkReg (True);

   rule rl_send_tv_reset (rg_just_after_reset);
      Bit #(64) serial_num = 0;
      fa_to_TV (way0, serial_num,
                tagged Invalid,
                no_deq_data, no_fflags, no_mstatus, no_trap_updates, no_ret_updates);
      rg_just_after_reset <= False;
      rg_serial_num       <= 1;
   endrule

   Reg #(Data) rg_old_mip_csr_val <- mkReg (0);

   Data new_mip_csr_val = inIfc.csrfIfc.getMIP;

   Bool send_mip_csr_change_to_tv = (new_mip_csr_val != rg_old_mip_csr_val);

   rule rl_send_mip_csr_change_to_tv ((! rg_just_after_reset) && send_mip_csr_change_to_tv);
      fa_to_TV (way0, rg_serial_num,
                tagged Valid (tuple2 (pack (csrAddrMIP), new_mip_csr_val)),
                no_deq_data, no_fflags, no_mstatus, no_trap_updates, no_ret_updates);
      rg_old_mip_csr_val <= new_mip_csr_val;
      rg_serial_num <= rg_serial_num + 1;
   endrule
`else
   Bool send_mip_csr_change_to_tv = False;
`endif

    // func units
    ReorderBufferSynth rob = inIfc.robIfc;
    RegRenamingTable regRenamingTable = inIfc.rtIfc;
    CsrFile csrf = inIfc.csrfIfc;

    // FIXME FIXME FIXME wires to set atCommit in LSQ: avoid scheduling cycle.
    // Using wire should be fine, because LSQ does not need to see atCommit
    // signal immediately. XXX The concern is about killAll which checks
    // atCommit in LSQ, but we never call killAll and setAtCommit in the same
    // cycle.
    Vector#(SupSize, RWire#(LdStQTag)) setLSQAtCommit <- replicateM(mkRWire);

`ifdef KONATA
    Vector#(SupSize, Ehr#(2, Maybe#(Bit#(64)))) printCommits <- replicateM(mkEhr(Invalid));
`endif

    for(Integer i = 0; i< valueof(SupSize); i = i+1) begin
        (* fire_when_enabled, no_implicit_conditions *)
        rule doSetLSQAtCommit(setLSQAtCommit[i].wget matches tagged Valid .tag);
            inIfc.lsqSetAtCommit[i].put(tag);
        endrule
    end

    // commit stage performance counters
`ifdef PERF_COUNT
    // inst
    Count#(Data) instCnt <- mkCount(0);
    Count#(Data) userInstCnt <- mkCount(0);
    Count#(Data) supComUserCnt <- mkCount(0);
    // branch/jump inst
    Count#(Data) comBrCnt <- mkCount(0);
    Count#(Data) comJmpCnt <- mkCount(0);
    Count#(Data) comJrCnt <- mkCount(0);
    // mem inst
    Count#(Data) comLdCnt <- mkCount(0);
    Count#(Data) comStCnt <- mkCount(0);
    Count#(Data) comLrCnt <- mkCount(0);
    Count#(Data) comScCnt <- mkCount(0);
    Count#(Data) comAmoCnt <- mkCount(0);
    // load mispeculation
    Count#(Data) comLdKillByLdCnt <- mkCount(0);
    Count#(Data) comLdKillByStCnt <- mkCount(0);
    Count#(Data) comLdKillByCacheCnt <- mkCount(0);
    // exception/sys inst related
    Count#(Data) comSysCnt <- mkCount(0);
    Count#(Data) excepCnt <- mkCount(0);
    Count#(Data) interruptCnt <- mkCount(0);
    // flush tlb
    Count#(Data) flushTlbCnt <- mkCount(0);
    // flush security
    Count#(Data) flushSecurityCnt <- mkCount(0);
    Count#(Data) flushBPCnt <- mkCount(0);
    Count#(Data) flushCacheCnt <- mkCount(0);
`endif

`ifdef PERFORMANCE_MONITORING
    Reg#(EventsCore) events_reg <- mkDReg(unpack(0));
`endif

`ifdef RVFI
    // RVFI trace report. Not an input?
    FIFO#(Rvfi_Traces) rvfiQ <- mkFIFO;
    Reg#(Dii_Id) traceCnt <- mkReg(0);

    function TraceStateBundle getTSB();
        return TraceStateBundle{
            sepc:  csrf.rd(csrAddrSEPC),
            mepc:  csrf.rd(csrAddrMEPC),
            stvec: csrf.rd(csrAddrSTVEC),
            mtvec: csrf.rd(csrAddrMTVEC)
        };
    endfunction
`endif

    // deadlock check
`ifdef CHECK_DEADLOCK
    // timer to check deadlock
    Reg#(DeadlockTimer) commitInstTimer <- mkReg(0);
    Reg#(DeadlockTimer) commitUserInstTimer <- mkReg(0);
    // FIFOs to output deadlock info
    FIFO#(CommitStuck) commitInstStuckQ <- mkFIFO1;
    FIFO#(CommitStuck) commitUserInstStuckQ <- mkFIFO1;
    // wires to indicate that deadlock is reported, so reset timers
    PulseWire commitInstStuckSent <- mkPulseWire;
    PulseWire commitUserInstStuckSent <- mkPulseWire;
    // wires to reset timers since processor is making progress
    PulseWire commitInst <- mkPulseWire;
    PulseWire commitUserInst <- mkPulseWire;

    function CommitStuck commitStuck;
        let x = rob.deqPort[0].deq_data;
        return CommitStuck {
            pc: x.pc,
            iType: x.iType,
            trap: x.trap,
            state: x.rob_inst_state,
            claimedPhyReg: x.claimed_phy_reg,
            ldKilled: isValid(x.ldKilled),
            memAccessAtCommit: x.memAccessAtCommit,
            lsqAtCommitNotified: x.lsqAtCommitNotified,
            nonMMIOStDone: x.nonMMIOStDone,
            epochIncremented: x.epochIncremented,
            specBits: x.spec_bits,
            stbEmpty: inIfc.stbEmpty,
            stqEmpty: inIfc.stqEmpty,
            tlbNoPendingReq: inIfc.tlbNoPendingReq,
            prv: csrf.decodeInfo.prv,
            instCount: csrf.rd(csrAddrINSTRET)
        };
    endfunction

    (* fire_when_enabled *)
    rule checkDeadlock_commitInst(inIfc.checkDeadlock && commitInstTimer == maxBound);
        commitInstStuckQ.enq(commitStuck);
        commitInstStuckSent.send;
    endrule

    (* fire_when_enabled *)
    rule checkDeadlock_commitUserInst(inIfc.checkDeadlock && commitUserInstTimer == maxBound);
        commitUserInstStuckQ.enq(commitStuck);
        commitUserInstStuckSent.send;
    endrule

    (* fire_when_enabled, no_implicit_conditions *)
    rule incrDeadlockTimer(inIfc.checkDeadlock);
        function DeadlockTimer getNextTimer(DeadlockTimer t);
            return t == maxBound ? maxBound : t + 1;
        endfunction
        commitInstTimer <= (commitInst || commitInstStuckSent) ? 0 : getNextTimer(commitInstTimer);
        commitUserInstTimer <= (commitUserInst || commitUserInstStuckSent) ? 0 : getNextTimer(commitUserInstTimer);
    endrule
`endif

`ifdef RENAME_DEBUG
    // rename debug
    Reg#(Bool) renameDebugStarted <- mkConfigReg(False);
    Reg#(Maybe#(RenameErrInfo)) renameErrInfo <- mkConfigReg(Invalid);
    Bool canSetRenameErr = renameDebugStarted && renameErrInfo == Invalid; // only set err info once
    // only send 1 error msg
    FIFO#(RenameErrInfo) renameErrQ <- mkFIFO1;

    rule sendRenameErr(renameDebugStarted &&& renameErrInfo matches tagged Valid .info);
        renameErrQ.enq(info);
        renameDebugStarted <= False;
    endrule
`endif

    // we commit trap in two cycles: first cycle deq ROB and flush; second
    // cycle handles trap, redirect and handles system consistency
    Reg#(Maybe#(CommitTrap)) commitTrap <- mkReg(Invalid); // saves new pc here
    Bool pauseCommit = isValid(commitTrap) || inIfc.pauseCommit;

    FIFO#(RedirectInfo) redirectQ <- mkFIFO;
    FIFO#(RedirectInfo) redirectQ2 <- mkFIFO; // Nasty hack to avoid killAll killing the epoch change in the Fetch stage.

    // maintain system consistency when system state (CSR) changes or for security
    function Action makeSystemConsistent(Bool flushTlb,
                                         Bool flushSecurity,
                                         Bool reconcileI);
    action
`ifndef SECURITY
        flushSecurity = False;
`endif

`ifndef DISABLE_SECURE_FLUSH_TLB
        if(flushTlb || flushSecurity) begin
`else
        if(flushTlb) begin
`endif
            inIfc.setFlushTlbs;
`ifdef PERF_COUNT
            if(inIfc.doStats) begin
                flushTlbCnt.incr(1);
            end
`endif
        end
        // notify TLB to keep update of CSR changes
        inIfc.setUpdateVMInfo;
        // always wait store buffer and SQ to be empty
        when(inIfc.stbEmpty && inIfc.stqEmpty, noAction);
        // We wait TLB to finish all requests and become sync with memory.
        // Notice that currently TLB is read only, so TLB is always in sync
        // with memory (i.e., there is no write to commit to memory). Since all
        // insts have been killed, nothing can be issued to D TLB at this time.
        // Since fetch stage is set to wait for redirect, fetch1 stage is
        // stalled, and nothing can be issued to I TLB at this time.
        // Therefore, we just need to make sure that I and D TLBs are not
        // handling any miss req. Besides, when I and D TLBs do not have any
        // miss req, L2 TLB must be idling.
        when(inIfc.tlbNoPendingReq, noAction);
        // yield load reservation in cache
        inIfc.setFlushReservation;

        // flush for security, we can delay the stall for fetch-empty and
        // wrong-path-load-empty until we really do the flush. This delay is
        // valid because these wrong path inst/req will not interfere with
        // whatever CSR changes we are making now.
        if(flushSecurity) begin
`ifdef PERF_COUNT
            if(inIfc.doStats) begin
                flushSecurityCnt.incr(1);
            end
`endif

`ifndef DISABLE_SECURE_FLUSH_BP
            inIfc.setFlushBrPred;
`ifdef PERF_COUNT
            if(inIfc.doStats) begin
                flushBPCnt.incr(1);
            end
`endif
`endif

`ifndef DISABLE_SECURE_FLUSH_CACHE
            inIfc.setFlushCaches;
`ifdef PERF_COUNT
            if(inIfc.doStats) begin
                flushCacheCnt.incr(1);
            end
`endif
`endif
        end

`ifdef SELF_INV_CACHE
        // reconcile I$
        if(reconcileI) begin
            inIfc.setReconcileI;
        end
`ifdef SYSTEM_SELF_INV_L1D
        // FIXME is this reconcile of D$ necessary?
        inIfc.setReconcileD;
`endif
`endif
    endaction
    endfunction

`ifdef INCLUDE_GDB_CONTROL
   // Maintain system consistency when halting into debug mode
   // This code is patterned after 'makeSystemConsistent' above
   function Action makeSystemConsistent_for_debug_mode;
      action
         inIfc.setFlushTlbs;

         // notify TLB to keep update of CSR changes
         inIfc.setUpdateVMInfo;
         // always wait store buffer and SQ to be empty
         when(inIfc.stbEmpty && inIfc.stqEmpty, noAction);
         // We wait TLB to finish all requests and become sync with memory.
         // Notice that currently TLB is read only, so TLB is always in sync
         // with memory (i.e., there is no write to commit to memory). Since all
         // insts have been killed, nothing can be issued to D TLB at this time.
         // Since fetch stage is set to wait for redirect, fetch1 stage is
         // stalled, and nothing can be issued to I TLB at this time.
         // Therefore, we just need to make sure that I and D TLBs are not
         // handling any miss req. Besides, when I and D TLBs do not have any
         // miss req, L2 TLB must be idling.
         when(inIfc.tlbNoPendingReq, noAction);
         // yield load reservation in cache
         inIfc.setFlushReservation;

         inIfc.setFlushBrPred;
         inIfc.setFlushCaches;

`ifdef SELF_INV_CACHE
         // reconcile I$
         if(reconcileI) begin
            inIfc.setReconcileI;
         end
`ifdef SYSTEM_SELF_INV_L1D
         // FIXME is this reconcile of D$ necessary?
         inIfc.setReconcileD;
`endif
`endif
      endaction
   endfunction
`endif

    // TODO Currently we don't check spec bits == 0 when we commit an
    // instruction. This is because killings of wrong path instructions are
    // done in a single cycle. However, when we make killings distributed or
    // pipelined, then we need to check spec bits at commit port.

    rule doCommitTrap_flush(
`ifdef INCLUDE_GDB_CONTROL
        (rg_run_state == RUN_STATE_RUNNING) &&&
`endif
        !pauseCommit &&&
        rob.deqPort[0].deq_data.trap matches tagged Valid .trap
    );
        $finish(0);
        rob.deqPort[0].deq;
        let x = rob.deqPort[0].deq_data;
        if(verbose) $display("[doCommitTrap] ", fshow(x));

        // record trap info
        Addr vaddr = 0;
        if(x.ppc_vaddr_csrData matches tagged VAddr .va) begin
            vaddr = va;
        end
        let commitTrap_val = Valid (CommitTrap {
            trap: trap,
            pc: x.pc,
            addr: vaddr,
            orig_inst: x.orig_inst
`ifdef RVFI
            , x: x
`endif
        });
        commitTrap <= commitTrap_val;
`ifdef INCLUDE_TANDEM_VERIF
        f_rob_data.enq (x);    // Save data to be sent to TV in rule doCommitTrap_handle, next
`endif
`ifdef KONATA
        $display("KONATAL\t%0d\t%0d\t0\tTrap %x", cur_cycle, x.u_id, x.pc);
        $display("KONATAR\t%0d\t%0d\t%0d\t1", cur_cycle, x.u_id, x.u_id);
        $fflush;
`endif
        if (verbosity >= 1) begin
           $display ("instret:%0d  PC:0x%0h  instr:0x%08h", rg_serial_num, x.pc, x.orig_inst,
                     "  iType:", fshow (x.iType), "    [doCommitTrap] %d", cur_cycle);
        end
        if (verbose) begin
           $display ("CommitStage.doCommitTrap_flush: deq_data:   ", fshow (x));
           $display ("CommitStage.doCommitTrap_flush: commitTrap: ", fshow (commitTrap_val));
        end

        // flush everything. Only increment epoch and stall fetch when we haven
        // not done it yet (we may have already done them at rename stage)
        inIfc.killAll;
        if(!x.epochIncremented) begin
            inIfc.incrementEpoch;
            inIfc.setFetchWaitRedirect;
        end

        // faulting mem inst may have claimed phy reg, we should not commit it;
        // instead, we kill the renaming by calling killAll

`ifdef PERF_COUNT
        // performance counter
        if(inIfc.doStats) begin
            if(trap matches tagged Exception .e) begin
                excepCnt.incr(1);
            end
            else begin
                interruptCnt.incr(1);
            end
        end
`endif
`ifdef PERFORMANCE_MONITORING
        EventsCore events = unpack(0);
        events.evt_TRAP = 1;
        if(trap matches tagged Interrupt .i) begin
            events.evt_INTERRUPT = 1;
        end
        events_reg <= events;
`endif
        // checks
        doAssert(x.rob_inst_state == Executed, "must be executed");
        doAssert(x.spec_bits == 0, "cannot have spec bits");
    endrule

    rule doCommitTrap_handle(
       commitTrap matches tagged Valid .trap
`ifdef INCLUDE_GDB_CONTROL
       &&& (rg_run_state == RUN_STATE_RUNNING)
`endif
       &&& (! send_mip_csr_change_to_tv));

        // reset commitTrap
        commitTrap <= Invalid;

`ifdef INCLUDE_TANDEM_VERIF
        let x = f_rob_data.first;
        f_rob_data.deq;
`endif

        // notify commit of interrupt (so MMIO pRq may be handled)
        if(trap.trap matches tagged Interrupt .inter) begin
            inIfc.commitCsrInstOrInterrupt;
        end

`ifdef INCLUDE_GDB_CONTROL
        else if (trap.trap == tagged Exception excBreakpoint) begin
            inIfc.commitCsrInstOrInterrupt;    // TODO: Why?
        end
`endif

`ifdef INCLUDE_GDB_CONTROL
       if ((trap.trap == tagged Interrupt intrDebugHalt)
           || (trap.trap == tagged Interrupt intrDebugStep)
           || ((trap.trap == tagged Exception excBreakpoint) && (csrf.dcsr_break_bit == 1'b1)))
          begin
             // Flush everything (tlbs, caches, reservation, branch predictor);
             // reconcilei and I; update VM info.
             makeSystemConsistent_for_debug_mode;

             // Save values in debugger CSRs
             Bit #(3) dcsr_cause = (  (trap.trap == tagged Interrupt intrDebugHalt)
                                    ? 3
                                    : (  (trap.trap == tagged Interrupt intrDebugStep)
                                       ? 4
                                       : 1));
             csrf.dcsr_cause_write (dcsr_cause);
             csrf.dpc_write (cast(trap.pc));

             // Tell fetch stage to wait for redirect
             // Note: rule doCommitTrap_flush may have done this already; redundant call is ok.
             // Or not?  These apparently conflict with redirectPC now?
             inIfc.setFetchWaitRedirect;
             inIfc.setFetchWaitFlush;

             // Go to quiescent state until debugger resumes execution
             rg_run_state <= RUN_STATE_DEBUGGER_HALTED;

             if (verbosity >= 2)
                $display ("%0d: %m.commitStage.doCommitTrap_handle; debugger halt:", cur_cycle);
          end else begin
`endif
       // trap handling & redirect
       let trap_updates <- csrf.trap(trap.trap, cast(trap.pc), trap.addr, trap.orig_inst);
       CapPipe new_pc = cast(trap_updates.new_pcc);
       redirectQ.enq(RedirectInfo{trap_pc: cast(new_pc)
`ifdef RVFI_DII
                                  , dii_pid: trap.x.dii_pid + (is_16b_inst(trap.orig_inst) ? 1 : 2)
`endif
       });
`ifdef RVFI
       Rvfi_Traces rvfis = replicate(tagged Invalid);
       rvfis[0] = genRVFI(trap.x, traceCnt, getTSB(), getAddr(new_pc), inIfc.lookupPAddr[0].sub(trap.x.lsqTag));
       rvfiQ.enq(rvfis);
       traceCnt <= traceCnt + 1;
`endif

`ifdef INCLUDE_TANDEM_VERIF
       fa_to_TV (way0, rg_serial_num,
                 tagged Invalid,
                 x, no_fflags, no_mstatus, tagged Valid trap_updates, no_ret_updates);
`endif
       rg_serial_num <= rg_serial_num + 1;

       // system consistency
       // TODO spike flushes TLB here, but perhaps it is because spike's TLB
       // does not include prv info, and it has to flush when prv changes.
       // XXX As approximation, Trap may cause context switch, so flush for
       // security
       makeSystemConsistent(False, True, False);
`ifdef INCLUDE_GDB_CONTROL
       end
`endif
    endrule

    // commit misspeculated load
    rule doCommitKilledLd(
`ifdef INCLUDE_GDB_CONTROL
       (rg_run_state == RUN_STATE_RUNNING) &&&
`endif
        !pauseCommit &&&
        !isValid(rob.deqPort[0].deq_data.trap) &&&
        rob.deqPort[0].deq_data.ldKilled matches tagged Valid .killBy
    );
        rob.deqPort[0].deq;
        let x = rob.deqPort[0].deq_data;
        if(verbose) $display("[doCommitKilledLd] ", fshow(x));

        // kill everything, redirect, and increment epoch
        inIfc.killAll;
        redirectQ.enq(RedirectInfo{trap_pc: x.pc
 `ifdef RVFI_DII
                                   , dii_pid: x.dii_pid
 `endif
        });
        inIfc.incrementEpoch;

        // the killed Ld should have claimed phy reg, we should not commit it;
        // instead, we have kill the renaming by calling killAll

`ifdef PERF_COUNT
        if(inIfc.doStats) begin
            case(killBy)
                Ld: comLdKillByLdCnt.incr(1);
                St: comLdKillByStCnt.incr(1);
                Cache: comLdKillByCacheCnt.incr(1);
            endcase
        end
`endif

        // checks
        doAssert(!x.epochIncremented, "cannot increment epoch before");
        doAssert(x.rob_inst_state == Executed, "must be executed");
        doAssert(x.spec_bits == 0, "cannot have spec bits");
    endrule

    // commit system inst
    rule doCommitSystemInst(
`ifdef INCLUDE_GDB_CONTROL
       (rg_run_state == RUN_STATE_RUNNING) &&
`endif
        !pauseCommit &&
        !isValid(rob.deqPort[0].deq_data.trap) &&
        !isValid(rob.deqPort[0].deq_data.ldKilled) &&
        rob.deqPort[0].deq_data.rob_inst_state == Executed &&
        isSystem(rob.deqPort[0].deq_data.iType) &&
        (! send_mip_csr_change_to_tv)
    );
        rob.deqPort[0].deq;
        let x = rob.deqPort[0].deq_data;

        if(verbose) $display("[doCommitSystemInst] ", fshow(x));
        if (verbosity >= 1) begin
           $display("instret:%0d  PC:0x%0h  instr:0x%08h", rg_serial_num, x.pc, x.orig_inst,
                    "   iType:", fshow (x.iType), "    [doCommitSystemInst] %d", cur_cycle);
        end

        // we claim a phy reg for every inst, so commit its renaming
        regRenamingTable.commit[0].commit;

`ifdef INCLUDE_TANDEM_VERIF
        Data new_mstatus = no_mstatus;
`endif

        Bool write_satp     = False; // flush tlb when satp csr is modified
        Bool flush_security = False; // flush for security when the flush csr is written
        if(x.iType == Csr) begin
            // notify commit of CSR (so MMIO pRq may be handled)
            inIfc.commitCsrInstOrInterrupt;
            // write CSR
            let csr_idx = validValue(x.csr);
            Data csr_data = ?;
            if(x.ppc_vaddr_csrData matches tagged CSRData .d) begin
                csr_data = getAddr(d);
            end
            else begin
                doAssert(False, "must have csr data");
            end
            csrf.csrInstWr(csr_idx, csr_data);

`ifdef INCLUDE_TANDEM_VERIF
            Data data_warl_xformed = csrf.warl_xform (csr_idx, csr_data);
            x.ppc_vaddr_csrData = tagged CSRData data_warl_xformed;

            if (x.will_dirty_fpu_state) begin
               Data old_mstatus = csrf.rd (csrAddrMSTATUS);
               new_mstatus = { 1'b1, old_mstatus [62:15], 2'b11, old_mstatus [12:0] };
            end
`endif

            // check if satp is modified or not
            write_satp = csr_idx == csrAddrSATP;
`ifdef SECURITY
            flush_security = csr_idx == csrAddrMFLUSH;
`endif
        end
        if(x.iType == Scr) begin
            // inIfc.commitCsrInstOrInterrupt; // TODO Will there be statcounter for SCRs?
            // write CSR
            let scr_idx = validValue(x.scr);
            CapMem scr_data = ?;
            if(x.ppc_vaddr_csrData matches tagged CSRData .d) begin
                scr_data = d;
            end
            else begin
                doAssert(False, "must have scr data");
            end
            csrf.scrInstWr(scr_idx, cast(scr_data)); // TODO only needs a CapReg so we could avoid generating the CapPipe in the first place
        end

        // redirect (Sret and Mret redirect pc is got from CSRF)
        CapMem next_pc = x.ppc_vaddr_csrData matches tagged PPC .ppc ? ppc : addPc(x.pc, 4);
        doAssert(getAddr(next_pc) == getAddr(x.pc) + 4, "ppc must be pc + 4");
`ifdef INCLUDE_TANDEM_VERIF
        Maybe #(RET_Updates) m_ret_updates = no_ret_updates;
`endif
        if(x.iType == Sret) begin
           RET_Updates ret_updates <- csrf.sret;
           next_pc = cast(ret_updates.new_pcc);
`ifdef INCLUDE_TANDEM_VERIF
           m_ret_updates = tagged Valid ret_updates;
`endif
        end
        else if(x.iType == Mret) begin
           RET_Updates ret_updates <- csrf.mret;
           next_pc = cast(ret_updates.new_pcc);
`ifdef INCLUDE_TANDEM_VERIF
           m_ret_updates = tagged Valid ret_updates;
`endif
        end
        redirectQ.enq(RedirectInfo{trap_pc: next_pc
 `ifdef RVFI_DII
                                   , dii_pid: x.dii_pid + (is_16b_inst(x.orig_inst) ? 1 : 2)
 `endif
        });


`ifdef RVFI
        Rvfi_Traces rvfis = replicate(tagged Invalid);
        x.ppc_vaddr_csrData = tagged PPC next_pc;
        CapPipe cp = cast(next_pc);
        rvfis[0] = genRVFI(x, traceCnt, getTSB(), getOffset(cp), inIfc.lookupPAddr[0].sub(x.lsqTag));
        rvfiQ.enq(rvfis);
        traceCnt <= traceCnt + 1;
`endif

`ifdef INCLUDE_TANDEM_VERIF
        fa_to_TV (way0, rg_serial_num,
                  tagged Invalid,
                  x, no_fflags, new_mstatus, no_trap_updates, m_ret_updates);
`endif
        rg_serial_num <= rg_serial_num + 1;

        // rename stage only sends out system inst when ROB is empty, so no
        // need to flush ROB again

        // system consistency
        // flush TLB for SFence.VMA and when SATP CSR is modified
        // XXX as approximation, sret/mret may mean context switch, so flush
        // for security
        makeSystemConsistent(
            x.iType == SFence || write_satp, // TODO flush TLB when change sanctum regs?
            flush_security || x.iType == Sret || x.iType == Mret,
            x.iType == FenceI // reconcile I$ for fence.i
        );

        // incr inst cnt
        csrf.incInstret(1);
`ifdef KONATA
        $display("KONATAE\t%0d\t%0d\t0\tAlu4", cur_cycle, x.u_id);
        $display("KONATAS\t%0d\t%0d\t0\tC", cur_cycle, x.u_id);
        $fflush;
        printCommits[0][1] <= tagged Valid x.u_id;
`endif
`ifdef PERF_COUNT
        if(inIfc.doStats) begin
            comSysCnt.incr(1);
            // inst count stats
            instCnt.incr(1);
            if(csrf.decodeInfo.prv == prvU) begin
                userInstCnt.incr(1);
            end
        end
`endif
`ifdef PERFORMANCE_MONITORING
        EventsCore events = unpack(0);
        case(x.iType)
            Fence, FenceI, SFence: events.evt_FENCE = 1;
        endcase
        events_reg <= events;
`endif
`ifdef CHECK_DEADLOCK
        commitInst.send;
        if(csrf.decodeInfo.prv == 0) begin
            commitUserInst.send;
        end
`endif

        // checks
        doAssert(x.epochIncremented, "must have already incremented epoch");
        doAssert((x.iType == Csr) == isValid(x.csr), "only CSR has valid csr idx");
        // RSN 2020-03-08: Removed this assertion. Csr instrs that write to
        // fflags/frm/fcsr do indeed 'dirty' the fpu state
        // doAssert(x.fflags == 0 && !x.will_dirty_fpu_state, "cannot dirty FPU");
        doAssert(x.spec_bits == 0, "cannot have spec bits");
        doAssert(x.claimed_phy_reg, "must have claimed phy reg");
`ifdef RENAME_DEBUG
        if(!x.claimed_phy_reg && canSetRenameErr) begin
            renameErrInfo <= Valid (RenameErrInfo {
                err: NonTrapCommitLackClaim,
                pc: x.pc,
                iType: x.iType,
                trap: x.trap,
                specBits: x.spec_bits
            });
        end
`endif
    endrule

    // Lr/Sc/Amo/MMIO cannot proceed to executed until we notify LSQ that it
    // has reached the commit stage
    rule notifyLSQCommit(
        !pauseCommit &&
        !isValid(rob.deqPort[0].deq_data.trap) &&
        !isValid(rob.deqPort[0].deq_data.ldKilled) &&
        rob.deqPort[0].deq_data.rob_inst_state != Executed &&
        rob.deqPort[0].deq_data.memAccessAtCommit &&
        !rob.deqPort[0].deq_data.lsqAtCommitNotified
    );
        let x = rob.deqPort[0].deq_data;
        let inst_tag = rob.deqPort[0].getDeqInstTag;
        if(verbose) $display("[notifyLSQCommit] ", fshow(x), "; ", fshow(inst_tag));

        // notify LSQ, and record in ROB that notification is done
        setLSQAtCommit[0].wset(x.lsqTag);
        rob.setLSQAtCommitNotified(inst_tag);
    endrule

    // commit normal: fire when at least one commit can be done
    rule doCommitNormalInst(
`ifdef INCLUDE_GDB_CONTROL
       (rg_run_state == RUN_STATE_RUNNING) &&
`endif
        !pauseCommit &&
        !isValid(rob.deqPort[0].deq_data.trap) &&
        !isValid(rob.deqPort[0].deq_data.ldKilled) &&
        rob.deqPort[0].deq_data.rob_inst_state == Executed &&
        !isSystem(rob.deqPort[0].deq_data.iType) &&
        (! send_mip_csr_change_to_tv)
    );
        // stop superscalar commit after we
        // 1. see a trap or system inst or killed Ld
        // 2. inst is not ready to commit
        Bool stop = False;

        // We merge writes on FPU csr and apply writes at the end of the rule
        Bit#(5) fflags = 0;
        Bool will_dirty_fpu_state = False;
        // rename error
        Maybe#(RenameErrInfo) renameError = Invalid;
        // incr committed inst cnt at the end of rule
        SupCnt comInstCnt = 0;
        SupCnt comUserInstCnt = 0;
        // incr some performance counter at the end of rule
        SupCnt brCnt = 0;
        SupCnt jmpCnt = 0;
        SupCnt jrCnt = 0;
        SupCnt ldCnt = 0;
        SupCnt stCnt = 0;
        SupCnt lrCnt = 0;
        SupCnt scCnt = 0;
        SupCnt amoCnt = 0;
        SupCnt shiftCnt = 0;
        SupCnt muldivCnt = 0;
        SupCnt auipcCnt = 0;
        SupCnt fenceCnt = 0;
        SupCnt fpuCnt = 0;
        // CHERI-specific counters
        SupCnt ldCapCnt = 0;
        SupCnt stCapCnt = 0;

`ifdef RVFI
        Rvfi_Traces rvfis = replicate(tagged Invalid);
        SupCnt whichTrace = 0;
`endif

        Bit #(64) instret = 0;

`ifdef INCLUDE_TANDEM_VERIF
       // These variables accumulate fflags and mstatus in sequential Program Order ('po')
       // (whereas the 'fflags' variable does just one update after superscalar retirement).
       Bit #(5) po_fflags  = ?;
       Data     po_mstatus = ?;
`endif

`ifdef PERFORMANCE_MONITORING
`ifdef CONTRACTS_VERIFY
        // update targets vector
        Vector#(SupSize, Maybe#(CapMem)) targets;
        // update return targets vector
        Vector#(SupSize, Maybe#(CapMem)) returnTargets;
`endif
`endif
        // compute what actions to take
        for(Integer i = 0; i < valueof(SupSize); i = i+1) begin
`ifdef PERFORMANCE_MONITORING
`ifdef CONTRACTS_VERIFY
            Maybe#(CapMem) tar = tagged Invalid;
            Maybe#(CapMem) retTar = tagged Invalid;
`endif
`endif
            if(!stop && rob.deqPort[i].canDeq) begin
                let x = rob.deqPort[i].deq_data;
                let inst_tag = rob.deqPort[i].getDeqInstTag;

                // check can be committed or not
                if(x.rob_inst_state != Executed || isValid(x.ldKilled) || isValid(x.trap) || isSystem(x.iType)) begin
                    // inst not ready for commit, or system inst, or trap, or killed, stop here
                    stop = True;
                end
                else begin
                    if (verbose) $display("%t : [doCommitNormalInst - %d] ", $time(), i, fshow(inst_tag), " ; ", fshow(x));
`ifdef RVFI
                    CapPipe pipePc = cast(x.pc);
                    rvfis[i] = genRVFI(x, traceCnt + zeroExtend(whichTrace), getTSB(), getAddr(pipePc) + (is_16b_inst(x.orig_inst) ? 2:4), inIfc.lookupPAddr[i].sub(x.lsqTag));
                    whichTrace = whichTrace + 1;
`endif

                    if (verbosity >= 1) begin
                       $display("instret:%0d  PC:0x%0h  instr:0x%08h", rg_serial_num + instret, x.pc, x.orig_inst,
                                "   iType:", fshow (x.iType), "    [doCommitNormalInst [%0d]] %d", i, cur_cycle);
                    end

`ifdef INCLUDE_TANDEM_VERIF
                   Bool init_for_way0 = (i == 0);
                   match {. new_fflags, .new_mstatus} = csrf.fpuInst_csr_updates (x.fflags,
                                                                                  init_for_way0,
                                                                                  po_fflags,
                                                                                  po_mstatus);
                   po_fflags  = new_fflags;
                   po_mstatus = new_mstatus;

                   fa_to_TV (i, rg_serial_num + instret,
                             tagged Invalid,
                             x,
                             po_fflags,
                             po_mstatus,
                             no_trap_updates, no_ret_updates);
`endif
                    instret = instret + 1;

                    // inst can be committed, deq it
                    rob.deqPort[i].deq;

`ifdef PERFORMANCE_MONITORING
`ifdef CONTRACTS_VERIFY
                    // return address stack link reg is x1 or x5
                    function Bool linkedR(Maybe#(ArchRIndx) register);
                        Bool res = False;
                        if (register matches tagged Valid .r &&& (r == tagged Gpr 1 || r == tagged Gpr 5)) begin
                           res = True;
                        end
                        return res;
                    endfunction
                    function Bool is_16b_inst (Bit #(n) inst);
                        return (inst [1:0] != 2'b11);
                    endfunction

                    // update return target
                    if(x.iType == J || x.iType == CJAL || x.iType == CJALR || x.iType == Jr) begin
                        tar = tagged Valid x.ppc_vaddr_csrData.PPC;
                        if(linkedR(x.dst)) begin
                            let imm = is_16b_inst(x.orig_inst) ? 2 : 4;
                            retTar = tagged Valid addPc(x.pc, imm);
                        end
                    end
`endif
`endif

                    // every inst here should have been renamed, commit renaming
                    regRenamingTable.commit[i].commit;
                    doAssert(x.claimed_phy_reg, "should have renamed");

`ifdef RENAME_DEBUG
                    // send debug msg for rename error
                    if(!x.claimed_phy_reg && !isValid(renameError)) begin
                        renameError = Valid (RenameErrInfo {
                            err: NonTrapCommitLackClaim,
                            pc: x.pc,
                            iType: x.iType,
                            trap: x.trap,
                            specBits: x.spec_bits
                        });
                    end
`endif

                    // cumulate writes to FPU csr
                    fflags = fflags | x.fflags;
                    will_dirty_fpu_state = will_dirty_fpu_state || x.will_dirty_fpu_state;

                    // for non-mmio st, notify SQ that store is committed
                    if(x.nonMMIOStDone) begin
                        setLSQAtCommit[i].wset(x.lsqTag);
                    end

                    // inst commit counter
                    comInstCnt = comInstCnt + 1;
                    if(csrf.decodeInfo.prv == 0) begin
                        comUserInstCnt = comUserInstCnt + 1; // user space inst
                    end

                    // performance counters
                    // Some fields of the original instruction to help with classification.
                    let inst = x.orig_inst;
                    Opcode opcode = unpackOpcode(inst[  6 :  0 ]);
                    let funct3    =              inst[ 14 : 12 ];
                    let funct7    =              inst[ 31 : 25 ];
                    // For "F" and "D" ISA extensions
                    let funct5    =              inst[ 31 : 27 ];
                    let fmt       =              inst[ 26 : 25 ];
                    let rs3       =              inst[ 31 : 27 ];
                    let funct2    =              inst[ 26 : 25 ];
                    // For "A" ISA extension
                    Bool aq       =       unpack(inst[ 26 ]);
                    Bool rl       =       unpack(inst[ 25 ]);
                    // For "xCHERI" ISA extension
                    let funct5rs2 =              inst[ 24 : 20 ];
                    case(x.iType)
                        Auipc, Auipcc: auipcCnt = auipcCnt + 1;
                        Br: brCnt = brCnt + 1;
                        J : jmpCnt = jmpCnt + 1;
                        CJAL: jmpCnt = jmpCnt + 1;
                        Jr: jrCnt = jrCnt + 1;
                        CJALR: jrCnt = jrCnt + 1;
                        Ld: begin
                            ldCnt = ldCnt + 1;
                        end
                        St: begin
                            stCnt = stCnt + 1;
                        end
                        Lr: lrCnt = lrCnt + 1;
                        Sc: scCnt = scCnt + 1;
                        Amo: amoCnt = amoCnt + 1;
                        Fpu: fpuCnt = fpuCnt + 1;
                        Alu: begin
                            if (((opcode == opcOpImm) || (opcode == opcOpImm32) || (opcode == opcOp)) && ((funct3 == fnSLL) || (funct3 == fnSR)))
                                shiftCnt = shiftCnt + 1;
                            if ((opcode == opcOp || opcode == opcOp32) && funct7 == opMULDIV)
                                muldivCnt = muldivCnt + 1;
                        end
                    endcase
                    if (opcode == opcMiscMem && funct3 == fnFENCE) fenceCnt = fenceCnt + 1;
`ifdef KONATA
                    case(x.iType)
                        Alu, J, Jr, Br, Auipc, Auipcc, CCall, CJAL, CJALR, Cap: begin
                            $display("KONATAE\t%0d\t%0d\t0\tAlu4", cur_cycle, x.u_id);
                            $display("KONATAS\t%0d\t%0d\t0\tC", cur_cycle, x.u_id);
                            $fflush; 
                        end
                        Ld, St, Lr, Sc, Amo: begin
                            $display("KONATAE\t%0d\t%0d\t0\tMem4", cur_cycle, x.u_id);
                            $display("KONATAS\t%0d\t%0d\t0\tC", cur_cycle, x.u_id);
                            $fflush; 
                        end
                        Fpu: begin
                            $display("KONATAE\t%0d\t%0d\t0\tFpu4", cur_cycle, x.u_id);
                            $display("KONATAS\t%0d\t%0d\t0\tC", cur_cycle, x.u_id);
                            $fflush; 
                        end
                    endcase
                    printCommits[i][1] <= tagged Valid x.u_id;
                    $fflush; 
`endif
                end
            end
`ifdef PERFORMANCE_MONITORING
`ifdef CONTRACTS_VERIFY
            targets[i] = tar;
            returnTargets[i] = retTar;
`endif
`endif
        end
        rg_serial_num <= rg_serial_num + instret;

        // write FPU csr
        if(csrf.fpuInstNeedWr(fflags, will_dirty_fpu_state)) begin
            csrf.fpuInstWr(fflags);
        end

        // incr inst cnt
        csrf.incInstret(comInstCnt);

`ifdef RENAME_DEBUG
        // set rename error
        if(canSetRenameErr && isValid(renameError)) begin
            renameErrInfo <= renameError;
        end
`endif

`ifdef CHECK_DEADLOCK
        commitInst.send; // ROB head is removed
        if(comUserInstCnt > 0) begin
            commitUserInst.send;
        end
`endif

`ifdef PERF_COUNT
        // performance counter
        if(inIfc.doStats) begin
            // branch stats
            comBrCnt.incr(zeroExtend(brCnt));
            comJmpCnt.incr(zeroExtend(jmpCnt));
            comJrCnt.incr(zeroExtend(jrCnt));
            // mem stats
            comLdCnt.incr(zeroExtend(ldCnt));
            comStCnt.incr(zeroExtend(stCnt));
            comLrCnt.incr(zeroExtend(lrCnt));
            comScCnt.incr(zeroExtend(scCnt));
            comAmoCnt.incr(zeroExtend(amoCnt));
            // inst count stats
            instCnt.incr(zeroExtend(comInstCnt));
            userInstCnt.incr(zeroExtend(comUserInstCnt));
            if(comUserInstCnt > 1) begin
                supComUserCnt.incr(1);
            end
        end
`endif
`ifdef PERFORMANCE_MONITORING
        EventsCore events = unpack(0);
        events.evt_BRANCH = zeroExtend(brCnt);
        events.evt_JAL = zeroExtend(jmpCnt);
        events.evt_JALR = zeroExtend(jrCnt);
        events.evt_AUIPC = zeroExtend(auipcCnt); // XXX
        events.evt_LOAD = zeroExtend(ldCnt);
        events.evt_STORE = zeroExtend(stCnt);
        events.evt_LR = zeroExtend(lrCnt);
        events.evt_SC = zeroExtend(scCnt);
        events.evt_AMO = zeroExtend(amoCnt);
        events.evt_SERIAL_SHIFT = zeroExtend(shiftCnt);
        events.evt_INT_MUL_DIV_REM = zeroExtend(muldivCnt);
        events.evt_FP = zeroExtend(fpuCnt);
        events.evt_FENCE = zeroExtend(fenceCnt);
        events_reg <= events;
`ifdef CONTRACTS_VERIFY
        inIfc.updateTargets(targets);
        inIfc.updateReturnTargets(returnTargets);
`endif
`endif

`ifdef RVFI
        rvfiQ.enq(rvfis);
        traceCnt <= traceCnt + zeroExtend(whichTrace);
`endif
    endrule

    rule pre_pass_redirect;
        redirectQ2.enq(redirectQ.first);
        redirectQ.deq;
    endrule

    rule pass_redirect;
        RedirectInfo ri <- toGet(redirectQ2).get;
        inIfc.redirectPc(ri.trap_pc, 0
`ifdef RVFI_DII
                         , ri.dii_pid
`endif
        );
    endrule

`ifdef KONATA
    rule doPrintCommitKONATA;
        for(Integer i = 0; i < valueof(SupSize); i = i + 1) begin
            if(printCommits[i][0] matches tagged Valid .u_id) begin
                $display("KCommit print");
                $display("KONATAE\t%0d\t%0d\t0\tC", cur_cycle, u_id);
                $display("KONATAR\t%0d\t%0d\t%0d\t0", cur_cycle, u_id, u_id);
                $fflush;
            end
        end
    endrule

    rule doMakePrintCommitInvalid;
        for(Integer i = 0; i < valueof(SupSize); i = i + 1) begin
            printCommits[i][0] <= Invalid;
        end
    endrule
`endif

   // ================================================================
   // INTERFACE

    method Data getPerf(ComStagePerfType t);
        return (case(t)
`ifdef PERF_COUNT
            InstCnt: instCnt;
            UserInstCnt: userInstCnt;
            SupComUserCnt: supComUserCnt;
            ComBrCnt: comBrCnt;
            ComJmpCnt: comJmpCnt;
            ComJrCnt: comJrCnt;
            ComLdCnt: comLdCnt;
            ComStCnt: comStCnt;
            ComLrCnt: comLrCnt;
            ComScCnt: comScCnt;
            ComAmoCnt: comAmoCnt;
            ComLdKillByLd: comLdKillByLdCnt;
            ComLdKillBySt: comLdKillByStCnt;
            ComLdKillByCache: comLdKillByCacheCnt;
            ComSysCnt: comSysCnt;
            ExcepCnt: excepCnt;
            InterruptCnt: interruptCnt;
            FlushTlbCnt: flushTlbCnt;
            FlushSecurityCnt: flushSecurityCnt;
            FlushBPCnt: flushBPCnt;
            FlushCacheCnt: flushCacheCnt;
`endif
            default: 0;
        endcase);
    endmethod

`ifdef RVFI
    method rvfi = toGet(rvfiQ);
`endif

`ifdef CHECK_DEADLOCK
    interface commitInstStuck = toGet(commitInstStuckQ);
    interface commitUserInstStuck = toGet(commitUserInstStuckQ);
`else
    interface commitInstStuck = nullGet;
    interface commitUserInstStuck = nullGet;
`endif

`ifdef RENAME_DEBUG
    method Action startRenameDebug if(!renameDebugStarted);
        renameDebugStarted <= True;
    endmethod
    interface renameErr = toGet(renameErrQ);
`else
    method Action startRenameDebug;
        noAction;
    endmethod
    interface renameErr = nullGet;
`endif

`ifdef INCLUDE_GDB_CONTROL
   method Bool is_debug_halted;
      return (rg_run_state == RUN_STATE_DEBUGGER_HALTED);
   endmethod

   method Action debug_resume () if (rg_run_state == RUN_STATE_DEBUGGER_HALTED);
      rg_run_state <= RUN_STATE_RUNNING;
      if (verbosity >= 2)
         $display ("%0d: %m.commitStage.debug_resume", cur_cycle);
   endmethod
`endif

`ifdef PERFORMANCE_MONITORING
   method events = events_reg;
`endif

endmodule
