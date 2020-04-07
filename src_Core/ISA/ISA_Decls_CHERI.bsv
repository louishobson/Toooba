/*
 * Copyright (c) 2019 Peter Rugg
 * All rights reserved.
 *
 * This software was developed by SRI International and the University of
 * Cambridge Computer Laboratory (Department of Computer Science and
 * Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
 * DARPA SSITH research programme.
 *
 * @BERI_LICENSE_HEADER_START@
 *
 * Licensed to BERI Open Systems C.I.C. (BERI) under one or more contributor
 * license agreements.  See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.  BERI licenses this
 * file to you under the BERI Hardware-Software License, Version 1.0 (the
 * "License"); you may not use this file except in compliance with the
 * License.  You may obtain a copy of the License at:
 *
 *   http://www.beri-open-systems.org/legal/license-1-0.txt
 *
 * Unless required by applicable law or agreed to in writing, Work distributed
 * under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
 * CONDITIONS OF ANY KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations under the License.
 *
 * @BERI_LICENSE_HEADER_END@
 */
import ISA_Decls::*;
import CHERICap::*;
import CHERICC_Fat::*;

typedef TMul#(XLEN, 2) CLEN;

// Exception codes

typedef enum {
    None                     = 5'd0,
    LengthViolation          = 5'd1,
    TagViolation             = 5'd2,
    SealViolation            = 5'd3,
    TypeViolation            = 5'd4,
    CallTrap                 = 5'd5,
    ReturnTrap               = 5'd6,
    StackUnderflow           = 5'd7,
    SoftwarePermViolation    = 5'd8,
    MMUStoreCapProhibit      = 5'd9,
    RepresentViolation       = 5'd10,
    UnalignedBase            = 5'd11,
    // 5'd12 - 5'd15 reserved
    GlobalViolation          = 5'd16,
    PermitXViolation         = 5'd17,
    PermitRViolation         = 5'd18,
    PermitWViolation         = 5'd19,
    PermitRCapViolation      = 5'd20,
    PermitWCapViolation      = 5'd21,
    PermitWLocalCapViolation = 5'd22,
    PermitSealViolation      = 5'd23,
    PermitASRViolation       = 5'd24,
    PermitCCallViolation     = 5'd25,
    PermitUnsealViolation    = 5'd26,
    PermitSetCIDViolation    = 5'd27
    // 5'd28 - 5'd31 reserved
} CHERIException  deriving(Bits, Eq, FShow);

typedef struct {
    Bit #(6) cheri_exc_reg;
    CHERIException cheri_exc_code;
} CSR_XCapCause deriving(Bits, FShow);

function Bit#(64) xccsr_to_word(CSR_XCapCause xccsr);
    return zeroExtend({xccsr.cheri_exc_reg, pack(xccsr.cheri_exc_code), 3'b0, 1'b1, 1'b1});
endfunction

function Reg#(Bit#(64)) csr_capcause(Reg#(CSR_XCapCause) r);
    return (interface Reg;
        method Bit#(64) _read = xccsr_to_word(r._read);
        method Action _write(Bit#(64) x) =
            r._write(CSR_XCapCause{cheri_exc_reg: x[15:10], cheri_exc_code: unpack(x[9:5]) });
    endinterface);
endfunction

// SCR map

typedef enum {
    SCR_PCC       = 5'd00,
    SCR_DDC       = 5'd01,

    SCR_UTCC      = 5'd04,
    SCR_UTDC      = 5'd05,
    SCR_UScratchC = 5'd06,
    SCR_UEPCC     = 5'd07,

    SCR_STCC      = 5'd12,
    SCR_STDC      = 5'd13,
    SCR_SScratchC = 5'd14,
    SCR_SEPCC     = 5'd15,

    SCR_MTCC      = 5'd28,
    SCR_MTDC      = 5'd29,
    SCR_MScratchC = 5'd30,
    SCR_MEPCC     = 5'd31
} SCR deriving(Bits, Eq, FShow);

function CapPipe update_scr_via_csr (CapPipe old_scr, WordXL new_csr);
    let new_scr = setOffset(old_scr, new_csr);
    let ret = new_scr.value;
    if (!new_scr.exact || isSealed(old_scr)) begin
        ret = setValidCap(ret, False);
    end
    return ret;
endfunction

RegName cCallRD = 31;

// Instruction field encodings

// Top-level opcodes
Opcode   op_cap_Manip = 7'h5b;
//Opcode   op_cap_Mem   = 7'h0b; // Not yet implemented

// ================================================================
// op_cap_Manip opcode subdivision

// f3 selects between immediate and 3-reg instructions
Bit #(3) f3_cap_ThreeOp             = 3'h0;
Bit #(3) f3_cap_CIncOffsetImmediate = 3'h1;
Bit #(3) f3_cap_CSetBoundsImmediate = 3'h2;
// 3'h3-3'h7 unused

// ================================================================
// op_cap_ThreeOp opcode subdivision

// f7 selects between 3-reg operations

// 7'h00 unused
Bit #(7) f7_cap_CSpecialRW      = 7'h01;
// 7'h02-7'h07 unused
Bit #(7) f7_cap_CSetBounds      = 7'h08;
Bit #(7) f7_cap_CSetBoundsExact = 7'h09;
// 7'h0a unused
Bit #(7) f7_cap_CSeal           = 7'h0b;
Bit #(7) f7_cap_CUnseal         = 7'h0c;
Bit #(7) f7_cap_CAndPerm        = 7'h0d;
Bit #(7) f7_cap_CSetFlags       = 7'h0e;
Bit #(7) f7_cap_CSetOffset      = 7'h0f;
Bit #(7) f7_cap_CSetAddr        = 7'h10;
Bit #(7) f7_cap_CIncOffset      = 7'h11;
Bit #(7) f7_cap_CToPtr          = 7'h12;
Bit #(7) f7_cap_CFromPtr        = 7'h13;
Bit #(7) f7_cap_CSub            = 7'h14;
// 7'h15-7'h1c unused
Bit #(7) f7_cap_CBuildCap       = 7'h1d;
Bit #(7) f7_cap_CCopyType       = 7'h1e;
Bit #(7) f7_cap_CCSeal          = 7'h1f;
Bit #(7) f7_cap_CTestSubset     = 7'h20;
// 7'h21-7'hfb unused
Bit #(7) f7_cap_Stores          = 7'h7c;
Bit #(7) f7_cap_Loads           = 7'h7d;
Bit #(7) f7_cap_TwoSrc          = 7'h7e;
Bit #(7) f7_cap_TwoOp           = 7'h7f;

// ================================================================
// f7_cap_TwoSrc opcode subdivision

// rd selects between 2-reg operations

// 5'h00 unused
Bit #(5) rd_cap_CCall          = 5'h01;
// 5'h02-5'h1f unused

// ================================================================
// f7_cap_TwoOp opcode subdivision

// f5rs2 selects between 2-reg operations (f5rs2 instead of f5 because f5
//        is already used in RISC-V and is in a different position

Bit #(5) f5rs2_cap_CGetPerm    = 5'h00;
Bit #(5) f5rs2_cap_CGetType    = 5'h01;
Bit #(5) f5rs2_cap_CGetBase    = 5'h02;
Bit #(5) f5rs2_cap_CGetLen     = 5'h03;
Bit #(5) f5rs2_cap_CGetTag     = 5'h04;
Bit #(5) f5rs2_cap_CGetSealed  = 5'h05;
Bit #(5) f5rs2_cap_CGetOffset  = 5'h06;
Bit #(5) f5rs2_cap_CGetFlags   = 5'h07;
Bit #(5) f5rs2_cap_CRRL        = 5'h08;
Bit #(5) f5rs2_cap_CRAM        = 5'h09;
Bit #(5) f5rs2_cap_CMove       = 5'h0a;
Bit #(5) f5rs2_cap_CClearTag   = 5'h0b;
Bit #(5) f5rs2_cap_CJALR       = 5'h0c;
Bit #(5) f5rs2_cap_CClearReg   = 5'h0d;
// 5'h0e unused
Bit #(5) f5rs2_cap_CGetAddr    = 5'h0f;
Bit #(5) f5rs2_cap_CClearFPReg = 5'h10;
// 5'h11-5'h1f unused (5'h1f reserved for 1-reg instructions

// ================================================================
// f7_cap_{Load, Store} opcode subdivision

MemReqSize cap_mem_SIZE_B = 'h0;
MemReqSize cap_mem_SIZE_H = 'h1;
MemReqSize cap_mem_SIZE_W = 'h2;
MemReqSize cap_mem_SIZE_D = 'h3;
//MemReqSize f5rs2_cap_mem_SIZE_Q = 'h4; //TODO

Bit #(1) cap_mem_ddc = 1'h0;
Bit #(1) cap_mem_cap = 1'h1;

Bit #(1) cap_mem_unsigned = 1'h1;
Bit #(1) cap_mem_signed = 1'h0;

// ================================================================
// Other:

// Region in MISC_MEM for LQ
Bit #(3) f3_LQ = 3'h2;
Bit #(3) f3_SQ = 3'b100;

`ifdef RV64
Bit #(3) w_SIZE_CAP = f3_SQ;
Bit #(3) w_SIZE_MAX = f3_SQ;
`else //RV32
Bit #(3) w_SIZE_CAP = f3_SD;
Bit #(3) w_SIZE_MAX = f3_SD;
`endif

Bit #(3) f3_AMO_CAP = w_SIZE_CAP;