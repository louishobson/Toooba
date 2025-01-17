//
// Generated by Bluespec Compiler, version 2019.05.beta2 (build a88bf40db, 2019-05-24)
//
//
//
//
// Ports:
// Name                         I/O  size props
// RDY_enq                        O     1
// RDY_deq                        O     1
// first                          O   158
// RDY_first                      O     1
// RDY_specUpdate_incorrectSpeculation  O     1 const
// RDY_specUpdate_correctSpeculation  O     1 const
// CLK                            I     1 clock
// RST_N                          I     1 reset
// enq_x                          I   158
// specUpdate_incorrectSpeculation_kill_all  I     1
// specUpdate_incorrectSpeculation_kill_tag  I     4
// specUpdate_correctSpeculation_mask  I    12
// EN_enq                         I     1
// EN_deq                         I     1
// EN_specUpdate_incorrectSpeculation  I     1
// EN_specUpdate_correctSpeculation  I     1
//
// Combinational paths from inputs to outputs:
//   (specUpdate_incorrectSpeculation_kill_all,
//    specUpdate_incorrectSpeculation_kill_tag,
//    EN_deq,
//    EN_specUpdate_incorrectSpeculation) -> RDY_enq
//
//

`ifdef BSV_ASSIGNMENT_DELAY
`else
  `define BSV_ASSIGNMENT_DELAY
`endif

`ifdef BSV_POSITIVE_RESET
  `define BSV_RESET_VALUE 1'b1
  `define BSV_RESET_EDGE posedge
`else
  `define BSV_RESET_VALUE 1'b0
  `define BSV_RESET_EDGE negedge
`endif

module mkAluDispToRegFifo(CLK,
			  RST_N,

			  enq_x,
			  EN_enq,
			  RDY_enq,

			  EN_deq,
			  RDY_deq,

			  first,
			  RDY_first,

			  specUpdate_incorrectSpeculation_kill_all,
			  specUpdate_incorrectSpeculation_kill_tag,
			  EN_specUpdate_incorrectSpeculation,
			  RDY_specUpdate_incorrectSpeculation,

			  specUpdate_correctSpeculation_mask,
			  EN_specUpdate_correctSpeculation,
			  RDY_specUpdate_correctSpeculation);
  input  CLK;
  input  RST_N;

  // action method enq
  input  [157 : 0] enq_x;
  input  EN_enq;
  output RDY_enq;

  // action method deq
  input  EN_deq;
  output RDY_deq;

  // value method first
  output [157 : 0] first;
  output RDY_first;

  // action method specUpdate_incorrectSpeculation
  input  specUpdate_incorrectSpeculation_kill_all;
  input  [3 : 0] specUpdate_incorrectSpeculation_kill_tag;
  input  EN_specUpdate_incorrectSpeculation;
  output RDY_specUpdate_incorrectSpeculation;

  // action method specUpdate_correctSpeculation
  input  [11 : 0] specUpdate_correctSpeculation_mask;
  input  EN_specUpdate_correctSpeculation;
  output RDY_specUpdate_correctSpeculation;

  // signals for module outputs
  wire [157 : 0] first;
  wire RDY_deq,
       RDY_enq,
       RDY_first,
       RDY_specUpdate_correctSpeculation,
       RDY_specUpdate_incorrectSpeculation;

  // inlined wires
  wire [11 : 0] m_m_specBits_0_lat_1$wget;
  wire m_m_valid_0_lat_0$whas;

  // register m_m_row_0
  reg [145 : 0] m_m_row_0;
  wire [145 : 0] m_m_row_0$D_IN;
  wire m_m_row_0$EN;

  // register m_m_specBits_0_rl
  reg [11 : 0] m_m_specBits_0_rl;
  wire [11 : 0] m_m_specBits_0_rl$D_IN;
  wire m_m_specBits_0_rl$EN;

  // register m_m_valid_0_rl
  reg m_m_valid_0_rl;
  wire m_m_valid_0_rl$D_IN, m_m_valid_0_rl$EN;

  // ports of submodule m_m_deqP_ehr_dummy2_0
  wire m_m_deqP_ehr_dummy2_0$D_IN, m_m_deqP_ehr_dummy2_0$EN;

  // ports of submodule m_m_deqP_ehr_dummy2_1
  wire m_m_deqP_ehr_dummy2_1$D_IN, m_m_deqP_ehr_dummy2_1$EN;

  // ports of submodule m_m_specBits_0_dummy2_0
  wire m_m_specBits_0_dummy2_0$D_IN,
       m_m_specBits_0_dummy2_0$EN,
       m_m_specBits_0_dummy2_0$Q_OUT;

  // ports of submodule m_m_specBits_0_dummy2_1
  wire m_m_specBits_0_dummy2_1$D_IN,
       m_m_specBits_0_dummy2_1$EN,
       m_m_specBits_0_dummy2_1$Q_OUT;

  // ports of submodule m_m_valid_0_dummy2_0
  wire m_m_valid_0_dummy2_0$D_IN,
       m_m_valid_0_dummy2_0$EN,
       m_m_valid_0_dummy2_0$Q_OUT;

  // ports of submodule m_m_valid_0_dummy2_1
  wire m_m_valid_0_dummy2_1$D_IN,
       m_m_valid_0_dummy2_1$EN,
       m_m_valid_0_dummy2_1$Q_OUT;

  // rule scheduling signals
  wire CAN_FIRE_RL_m_m_specBits_0_canon,
       CAN_FIRE_RL_m_m_valid_0_canon,
       CAN_FIRE_deq,
       CAN_FIRE_enq,
       CAN_FIRE_specUpdate_correctSpeculation,
       CAN_FIRE_specUpdate_incorrectSpeculation,
       WILL_FIRE_RL_m_m_specBits_0_canon,
       WILL_FIRE_RL_m_m_valid_0_canon,
       WILL_FIRE_deq,
       WILL_FIRE_enq,
       WILL_FIRE_specUpdate_correctSpeculation,
       WILL_FIRE_specUpdate_incorrectSpeculation;

  // inputs to muxes for submodule ports
  wire MUX_m_m_valid_0_dummy2_0$write_1__SEL_1;

  // remaining internal signals
  reg [20 : 0] CASE_enq_x_BITS_152_TO_150_0_enq_x_BITS_152_TO_ETC__q2,
	       CASE_m_m_row_0_BITS_140_TO_138_0_m_m_row_0_BIT_ETC__q5;
  reg [11 : 0] CASE_enq_x_BITS_130_TO_119_1_enq_x_BITS_130_TO_ETC__q3,
	       CASE_m_m_row_0_BITS_118_TO_107_1_m_m_row_0_BIT_ETC__q6;
  reg [2 : 0] CASE_enq_x_BITS_135_TO_133_0_enq_x_BITS_135_TO_ETC__q1,
	      CASE_m_m_row_0_BITS_123_TO_121_0_m_m_row_0_BIT_ETC__q4;
  wire [11 : 0] IF_m_m_specBits_0_dummy2_0_read__02_AND_m_m_sp_ETC___d305,
		IF_m_m_specBits_0_lat_0_whas__0_THEN_m_m_specB_ETC___d13,
		sb__h10697,
		upd__h2327;
  wire IF_m_m_valid_0_lat_0_whas_THEN_m_m_valid_0_lat_ETC___d6;

  // action method enq
  assign RDY_enq =
	     !m_m_valid_0_dummy2_1$Q_OUT ||
	     (m_m_valid_0_lat_0$whas ? !1'd0 : !m_m_valid_0_rl) ;
  assign CAN_FIRE_enq =
	     !m_m_valid_0_dummy2_1$Q_OUT ||
	     (m_m_valid_0_lat_0$whas ? !1'd0 : !m_m_valid_0_rl) ;
  assign WILL_FIRE_enq = EN_enq ;

  // action method deq
  assign RDY_deq =
	     m_m_valid_0_dummy2_0$Q_OUT && m_m_valid_0_dummy2_1$Q_OUT &&
	     m_m_valid_0_rl ;
  assign CAN_FIRE_deq = RDY_deq ;
  assign WILL_FIRE_deq = EN_deq ;

  // value method first
  assign first =
	     { m_m_row_0[145:141],
	       CASE_m_m_row_0_BITS_140_TO_138_0_m_m_row_0_BIT_ETC__q5,
	       m_m_row_0[119],
	       CASE_m_m_row_0_BITS_118_TO_107_1_m_m_row_0_BIT_ETC__q6,
	       m_m_row_0[106:0],
	       IF_m_m_specBits_0_dummy2_0_read__02_AND_m_m_sp_ETC___d305 } ;
  assign RDY_first = RDY_deq ;

  // action method specUpdate_incorrectSpeculation
  assign RDY_specUpdate_incorrectSpeculation = 1'd1 ;
  assign CAN_FIRE_specUpdate_incorrectSpeculation = 1'd1 ;
  assign WILL_FIRE_specUpdate_incorrectSpeculation =
	     EN_specUpdate_incorrectSpeculation ;

  // action method specUpdate_correctSpeculation
  assign RDY_specUpdate_correctSpeculation = 1'd1 ;
  assign CAN_FIRE_specUpdate_correctSpeculation = 1'd1 ;
  assign WILL_FIRE_specUpdate_correctSpeculation =
	     EN_specUpdate_correctSpeculation ;

  // submodule m_m_deqP_ehr_dummy2_0
  RevertReg #(.width(32'd1), .init(1'd1)) m_m_deqP_ehr_dummy2_0(.CLK(CLK),
								.D_IN(m_m_deqP_ehr_dummy2_0$D_IN),
								.EN(m_m_deqP_ehr_dummy2_0$EN),
								.Q_OUT());

  // submodule m_m_deqP_ehr_dummy2_1
  RevertReg #(.width(32'd1), .init(1'd1)) m_m_deqP_ehr_dummy2_1(.CLK(CLK),
								.D_IN(m_m_deqP_ehr_dummy2_1$D_IN),
								.EN(m_m_deqP_ehr_dummy2_1$EN),
								.Q_OUT());

  // submodule m_m_specBits_0_dummy2_0
  RevertReg #(.width(32'd1), .init(1'd1)) m_m_specBits_0_dummy2_0(.CLK(CLK),
								  .D_IN(m_m_specBits_0_dummy2_0$D_IN),
								  .EN(m_m_specBits_0_dummy2_0$EN),
								  .Q_OUT(m_m_specBits_0_dummy2_0$Q_OUT));

  // submodule m_m_specBits_0_dummy2_1
  RevertReg #(.width(32'd1), .init(1'd1)) m_m_specBits_0_dummy2_1(.CLK(CLK),
								  .D_IN(m_m_specBits_0_dummy2_1$D_IN),
								  .EN(m_m_specBits_0_dummy2_1$EN),
								  .Q_OUT(m_m_specBits_0_dummy2_1$Q_OUT));

  // submodule m_m_valid_0_dummy2_0
  RevertReg #(.width(32'd1), .init(1'd1)) m_m_valid_0_dummy2_0(.CLK(CLK),
							       .D_IN(m_m_valid_0_dummy2_0$D_IN),
							       .EN(m_m_valid_0_dummy2_0$EN),
							       .Q_OUT(m_m_valid_0_dummy2_0$Q_OUT));

  // submodule m_m_valid_0_dummy2_1
  RevertReg #(.width(32'd1), .init(1'd1)) m_m_valid_0_dummy2_1(.CLK(CLK),
							       .D_IN(m_m_valid_0_dummy2_1$D_IN),
							       .EN(m_m_valid_0_dummy2_1$EN),
							       .Q_OUT(m_m_valid_0_dummy2_1$Q_OUT));

  // rule RL_m_m_valid_0_canon
  assign CAN_FIRE_RL_m_m_valid_0_canon = 1'd1 ;
  assign WILL_FIRE_RL_m_m_valid_0_canon = 1'd1 ;

  // rule RL_m_m_specBits_0_canon
  assign CAN_FIRE_RL_m_m_specBits_0_canon = 1'd1 ;
  assign WILL_FIRE_RL_m_m_specBits_0_canon = 1'd1 ;

  // inputs to muxes for submodule ports
  assign MUX_m_m_valid_0_dummy2_0$write_1__SEL_1 =
	     EN_specUpdate_incorrectSpeculation &&
	     (specUpdate_incorrectSpeculation_kill_all ||
	      IF_m_m_specBits_0_dummy2_0_read__02_AND_m_m_sp_ETC___d305[specUpdate_incorrectSpeculation_kill_tag]) ;

  // inlined wires
  assign m_m_valid_0_lat_0$whas =
	     MUX_m_m_valid_0_dummy2_0$write_1__SEL_1 || EN_deq ;
  assign m_m_specBits_0_lat_1$wget =
	     sb__h10697 & specUpdate_correctSpeculation_mask ;

  // register m_m_row_0
  assign m_m_row_0$D_IN =
	     { enq_x[157:153],
	       CASE_enq_x_BITS_152_TO_150_0_enq_x_BITS_152_TO_ETC__q2,
	       enq_x[131],
	       CASE_enq_x_BITS_130_TO_119_1_enq_x_BITS_130_TO_ETC__q3,
	       enq_x[118:12] } ;
  assign m_m_row_0$EN = EN_enq ;

  // register m_m_specBits_0_rl
  assign m_m_specBits_0_rl$D_IN =
	     EN_specUpdate_correctSpeculation ?
	       upd__h2327 :
	       IF_m_m_specBits_0_lat_0_whas__0_THEN_m_m_specB_ETC___d13 ;
  assign m_m_specBits_0_rl$EN = 1'd1 ;

  // register m_m_valid_0_rl
  assign m_m_valid_0_rl$D_IN =
	     EN_enq ||
	     IF_m_m_valid_0_lat_0_whas_THEN_m_m_valid_0_lat_ETC___d6 ;
  assign m_m_valid_0_rl$EN = 1'd1 ;

  // submodule m_m_deqP_ehr_dummy2_0
  assign m_m_deqP_ehr_dummy2_0$D_IN = 1'd1 ;
  assign m_m_deqP_ehr_dummy2_0$EN = EN_deq ;

  // submodule m_m_deqP_ehr_dummy2_1
  assign m_m_deqP_ehr_dummy2_1$D_IN = 1'b0 ;
  assign m_m_deqP_ehr_dummy2_1$EN = 1'b0 ;

  // submodule m_m_specBits_0_dummy2_0
  assign m_m_specBits_0_dummy2_0$D_IN = 1'd1 ;
  assign m_m_specBits_0_dummy2_0$EN = EN_enq ;

  // submodule m_m_specBits_0_dummy2_1
  assign m_m_specBits_0_dummy2_1$D_IN = 1'd1 ;
  assign m_m_specBits_0_dummy2_1$EN = EN_specUpdate_correctSpeculation ;

  // submodule m_m_valid_0_dummy2_0
  assign m_m_valid_0_dummy2_0$D_IN = 1'd1 ;
  assign m_m_valid_0_dummy2_0$EN =
	     MUX_m_m_valid_0_dummy2_0$write_1__SEL_1 || EN_deq ;

  // submodule m_m_valid_0_dummy2_1
  assign m_m_valid_0_dummy2_1$D_IN = 1'd1 ;
  assign m_m_valid_0_dummy2_1$EN = EN_enq ;

  // remaining internal signals
  assign IF_m_m_specBits_0_dummy2_0_read__02_AND_m_m_sp_ETC___d305 =
	     (m_m_specBits_0_dummy2_0$Q_OUT &&
	      m_m_specBits_0_dummy2_1$Q_OUT) ?
	       m_m_specBits_0_rl :
	       12'd0 ;
  assign IF_m_m_specBits_0_lat_0_whas__0_THEN_m_m_specB_ETC___d13 =
	     EN_enq ? enq_x[11:0] : m_m_specBits_0_rl ;
  assign IF_m_m_valid_0_lat_0_whas_THEN_m_m_valid_0_lat_ETC___d6 =
	     m_m_valid_0_lat_0$whas ? 1'd0 : m_m_valid_0_rl ;
  assign sb__h10697 =
	     m_m_specBits_0_dummy2_1$Q_OUT ?
	       IF_m_m_specBits_0_lat_0_whas__0_THEN_m_m_specB_ETC___d13 :
	       12'd0 ;
  assign upd__h2327 = m_m_specBits_0_lat_1$wget ;
  always@(enq_x)
  begin
    case (enq_x[135:133])
      3'd0, 3'd1, 3'd2, 3'd3, 3'd4:
	  CASE_enq_x_BITS_135_TO_133_0_enq_x_BITS_135_TO_ETC__q1 =
	      enq_x[135:133];
      default: CASE_enq_x_BITS_135_TO_133_0_enq_x_BITS_135_TO_ETC__q1 = 3'd7;
    endcase
  end
  always@(enq_x or CASE_enq_x_BITS_135_TO_133_0_enq_x_BITS_135_TO_ETC__q1)
  begin
    case (enq_x[152:150])
      3'd0, 3'd1, 3'd2, 3'd3:
	  CASE_enq_x_BITS_152_TO_150_0_enq_x_BITS_152_TO_ETC__q2 =
	      enq_x[152:132];
      3'd4:
	  CASE_enq_x_BITS_152_TO_150_0_enq_x_BITS_152_TO_ETC__q2 =
	      { enq_x[152:150],
		9'h0AA,
		enq_x[140:136],
		CASE_enq_x_BITS_135_TO_133_0_enq_x_BITS_135_TO_ETC__q1,
		enq_x[132] };
      default: CASE_enq_x_BITS_152_TO_150_0_enq_x_BITS_152_TO_ETC__q2 =
		   21'd1485482;
    endcase
  end
  always@(enq_x)
  begin
    case (enq_x[130:119])
      12'd1,
      12'd2,
      12'd3,
      12'd256,
      12'd260,
      12'd261,
      12'd262,
      12'd320,
      12'd321,
      12'd322,
      12'd323,
      12'd324,
      12'd384,
      12'd768,
      12'd769,
      12'd770,
      12'd771,
      12'd772,
      12'd773,
      12'd774,
      12'd832,
      12'd833,
      12'd834,
      12'd835,
      12'd836,
      12'd1952,
      12'd1953,
      12'd1954,
      12'd1955,
      12'd2048,
      12'd2049,
      12'd2816,
      12'd2818,
      12'd3072,
      12'd3073,
      12'd3074,
      12'd3857,
      12'd3858,
      12'd3859,
      12'd3860:
	  CASE_enq_x_BITS_130_TO_119_1_enq_x_BITS_130_TO_ETC__q3 =
	      enq_x[130:119];
      default: CASE_enq_x_BITS_130_TO_119_1_enq_x_BITS_130_TO_ETC__q3 =
		   12'd2303;
    endcase
  end
  always@(m_m_row_0)
  begin
    case (m_m_row_0[123:121])
      3'd0, 3'd1, 3'd2, 3'd3, 3'd4:
	  CASE_m_m_row_0_BITS_123_TO_121_0_m_m_row_0_BIT_ETC__q4 =
	      m_m_row_0[123:121];
      default: CASE_m_m_row_0_BITS_123_TO_121_0_m_m_row_0_BIT_ETC__q4 = 3'd7;
    endcase
  end
  always@(m_m_row_0 or CASE_m_m_row_0_BITS_123_TO_121_0_m_m_row_0_BIT_ETC__q4)
  begin
    case (m_m_row_0[140:138])
      3'd0, 3'd1, 3'd2, 3'd3:
	  CASE_m_m_row_0_BITS_140_TO_138_0_m_m_row_0_BIT_ETC__q5 =
	      m_m_row_0[140:120];
      3'd4:
	  CASE_m_m_row_0_BITS_140_TO_138_0_m_m_row_0_BIT_ETC__q5 =
	      { m_m_row_0[140:138],
		9'h0AA,
		m_m_row_0[128:124],
		CASE_m_m_row_0_BITS_123_TO_121_0_m_m_row_0_BIT_ETC__q4,
		m_m_row_0[120] };
      default: CASE_m_m_row_0_BITS_140_TO_138_0_m_m_row_0_BIT_ETC__q5 =
		   21'd1485482;
    endcase
  end
  always@(m_m_row_0)
  begin
    case (m_m_row_0[118:107])
      12'd1,
      12'd2,
      12'd3,
      12'd256,
      12'd260,
      12'd261,
      12'd262,
      12'd320,
      12'd321,
      12'd322,
      12'd323,
      12'd324,
      12'd384,
      12'd768,
      12'd769,
      12'd770,
      12'd771,
      12'd772,
      12'd773,
      12'd774,
      12'd832,
      12'd833,
      12'd834,
      12'd835,
      12'd836,
      12'd1952,
      12'd1953,
      12'd1954,
      12'd1955,
      12'd2048,
      12'd2049,
      12'd2816,
      12'd2818,
      12'd3072,
      12'd3073,
      12'd3074,
      12'd3857,
      12'd3858,
      12'd3859,
      12'd3860:
	  CASE_m_m_row_0_BITS_118_TO_107_1_m_m_row_0_BIT_ETC__q6 =
	      m_m_row_0[118:107];
      default: CASE_m_m_row_0_BITS_118_TO_107_1_m_m_row_0_BIT_ETC__q6 =
		   12'd2303;
    endcase
  end

  // handling of inlined registers

  always@(posedge CLK)
  begin
    if (RST_N == `BSV_RESET_VALUE)
      begin
        m_m_specBits_0_rl <= `BSV_ASSIGNMENT_DELAY 12'hAAA;
	m_m_valid_0_rl <= `BSV_ASSIGNMENT_DELAY 1'd0;
      end
    else
      begin
        if (m_m_specBits_0_rl$EN)
	  m_m_specBits_0_rl <= `BSV_ASSIGNMENT_DELAY m_m_specBits_0_rl$D_IN;
	if (m_m_valid_0_rl$EN)
	  m_m_valid_0_rl <= `BSV_ASSIGNMENT_DELAY m_m_valid_0_rl$D_IN;
      end
    if (m_m_row_0$EN) m_m_row_0 <= `BSV_ASSIGNMENT_DELAY m_m_row_0$D_IN;
  end

  // synopsys translate_off
  `ifdef BSV_NO_INITIAL_BLOCKS
  `else // not BSV_NO_INITIAL_BLOCKS
  initial
  begin
    m_m_row_0 = 146'h2AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA;
    m_m_specBits_0_rl = 12'hAAA;
    m_m_valid_0_rl = 1'h0;
  end
  `endif // BSV_NO_INITIAL_BLOCKS
  // synopsys translate_on

  // handling of system tasks

  // synopsys translate_off
  always@(negedge CLK)
  begin
    #0;
    if (RST_N != `BSV_RESET_VALUE)
      if (EN_enq && m_m_valid_0_dummy2_1$Q_OUT &&
	  IF_m_m_valid_0_lat_0_whas_THEN_m_m_valid_0_lat_ETC___d6)
	$fdisplay(32'h80000002, "\n%m: ASSERT FAIL!!");
    if (RST_N != `BSV_RESET_VALUE)
      if (EN_enq && m_m_valid_0_dummy2_1$Q_OUT &&
	  IF_m_m_valid_0_lat_0_whas_THEN_m_m_valid_0_lat_ETC___d6)
	$display("Dynamic assertion failed: \"../../src_Core/RISCY_OOO/procs/lib/SpecFifo.bsv\", line 147, column 34\nenq entry cannot be valid");
    if (RST_N != `BSV_RESET_VALUE)
      if (EN_enq && m_m_valid_0_dummy2_1$Q_OUT &&
	  IF_m_m_valid_0_lat_0_whas_THEN_m_m_valid_0_lat_ETC___d6)
	$finish(32'd0);
  end
  // synopsys translate_on
endmodule  // mkAluDispToRegFifo

