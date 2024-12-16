
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

import BRAMCore::*;
import Fifos::*;

interface RWBramCore#(type addrT, type dataT);
    method Action wrReq(addrT a, dataT d);
    method Action rdReq(addrT a);
    method dataT rdResp;
    method Bool rdRespValid;
    method Action deqRdResp;
endinterface

interface RBramCore#(type addrT, type dataT);
    method Action rd1Req(addrT a);
    method Action rd2Req(addrT a);
    method dataT rd1Resp;
    method dataT rd2Resp;
    method Bool rd1RespValid;
    method Bool rd2RespValid;
    method Action deqRd1Resp;
    method Action deqRd2Resp;
endinterface

module mkRWBramCore(RWBramCore#(addrT, dataT)) provisos(
    Bits#(addrT, addrSz), Bits#(dataT, dataSz)
);
    BRAM_DUAL_PORT#(addrT, dataT) bram <- mkBRAMCore2(valueOf(TExp#(addrSz)), False);
    BRAM_PORT#(addrT, dataT) wrPort = bram.a;
    BRAM_PORT#(addrT, dataT) rdPort = bram.b;
    // 1 elem pipeline fifo to add guard for read req/resp
    // must be 1 elem to make sure rdResp is not corrupted
    // BRAMCore should not change output if no req is made
    Fifo#(1, void) rdReqQ <- mkPipelineFifo;

    method Action wrReq(addrT a, dataT d);
        wrPort.put(True, a, d);
    endmethod

    method Action rdReq(addrT a);
        rdReqQ.enq(?);
        rdPort.put(False, a, ?);
    endmethod

    method dataT rdResp if(rdReqQ.notEmpty);
        return rdPort.read;
    endmethod

    method rdRespValid = rdReqQ.notEmpty;

    method Action deqRdResp;
        rdReqQ.deq;
    endmethod
endmodule

module mkRBramCore#(String fileName, Bool binary)(RBramCore#(addrT, dataT)) provisos(
    Bits#(addrT, addrSz), Bits#(dataT, dataSz)
);
    BRAM_DUAL_PORT#(addrT, dataT) bram <- mkBRAMCore2(valueOf(TExp#(addrSz)), False/*, fileName, binary*/);
    BRAM_PORT#(addrT, dataT) rd1Port = bram.a;
    BRAM_PORT#(addrT, dataT) rd2Port = bram.b;
    // 1 elem pipeline fifo to add guard for read req/resp
    // must be 1 elem to make sure rdResp is not corrupted
    // BRAMCore should not change output if no req is made
    Fifo#(1, void) rd1ReqQ <- mkPipelineFifo;
    Fifo#(1, void) rd2ReqQ <- mkPipelineFifo;

    method Action rd1Req(addrT a);
        rd1ReqQ.enq(?);
        rd1Port.put(False, a, ?);
    endmethod

    method Action rd2Req(addrT a);
        rd2ReqQ.enq(?);
        rd2Port.put(False, a, ?);
    endmethod

    method dataT rd1Resp if(rd1ReqQ.notEmpty);
        return rd1Port.read;
    endmethod

    method dataT rd2Resp if(rd2ReqQ.notEmpty);
        return rd2Port.read;
    endmethod

    method rd1RespValid = rd1ReqQ.notEmpty;
    method rd2RespValid = rd2ReqQ.notEmpty;

    method Action deqRd1Resp;
        rd1ReqQ.deq;
    endmethod

    method Action deqRd2Resp;
        rd2ReqQ.deq;
    endmethod
endmodule

module mkRWBramCoreForwarded(RWBramCore#(addrT, dataT)) provisos(
    Bits#(addrT, addrSz), Bits#(dataT, dataSz), Eq#(addrT)
);
    BRAM_DUAL_PORT#(addrT, dataT) bram <- mkBRAMCore2(valueOf(TExp#(addrSz)), False);
    BRAM_PORT#(addrT, dataT) wrPort = bram.a;
    BRAM_PORT#(addrT, dataT) rdPort = bram.b;
    // 1 elem pipeline fifo to add guard for read req/resp
    // must be 1 elem to make sure rdResp is not corrupted
    // BRAMCore should not change output if no req is made
    Fifo#(1, void) rdReqQ <- mkPipelineFifo;
    Reg#(addrT) readAddr[2] <- mkCReg(2,?);
    Reg#(addrT) currentWriteAddr <- mkRegU;
    Reg#(dataT) currentWriteData <- mkRegU;

    rule doRead;
        rdPort.put(False, readAddr[1], ?);
    endrule

    method Action wrReq(addrT a, dataT d);
        wrPort.put(True, a, d);
        currentWriteAddr <= a; //Forward data, if read happens on same cycle
        currentWriteData <= d;
    endmethod

    method Action rdReq(addrT a);
        readAddr[0] <= a;
        rdReqQ.enq(?);
    endmethod

    method dataT rdResp if(rdReqQ.notEmpty);
        return (readAddr[0] == currentWriteAddr) ? currentWriteData : rdPort.read;
    endmethod

    method rdRespValid = rdReqQ.notEmpty;

    method Action deqRdResp;
        rdReqQ.deq;
    endmethod
endmodule

module mkRWBramCoreUG(RWBramCore#(addrT, dataT)) provisos(
    Bits#(addrT, addrSz), Bits#(dataT, dataSz)
);
    BRAM_DUAL_PORT#(addrT, dataT) bram <- mkBRAMCore2(valueOf(TExp#(addrSz)), False);
    BRAM_PORT#(addrT, dataT) wrPort = bram.a;
    BRAM_PORT#(addrT, dataT) rdPort = bram.b;

    method Action wrReq(addrT a, dataT d);
        wrPort.put(True, a, d);
    endmethod

    method Action rdReq(addrT a);
        rdPort.put(False, a, ?);
    endmethod

    method dataT rdResp;
        return rdPort.read;
    endmethod

    method rdRespValid = True;

    method Action deqRdResp;
        noAction;
    endmethod
endmodule
