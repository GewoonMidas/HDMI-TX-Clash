/* AUTOMATICALLY GENERATED VERILOG-2001 SOURCE CODE.
** GENERATED BY CLASH 1.6.3. DO NOT MODIFY.
*/
`timescale 100fs/100fs
module bitmaster
    ( // Inputs
      input  clk // clock
    , input  arst // reset
    , input  gen // enable
    , input  rst
    , input  ena
    , input [15:0] clkCnt
    , input [2:0] cmd
    , input  din
    , input [1:0] i2cI

      // Outputs
    , output wire  cmdAck
    , output wire  al
    , output wire  dout
    , output wire  busy
    , output wire [3:0] i2cO
    );
  wire [62:0] result_1;
  // Hdmi/I2C/BitMaster.hs:76:1-10
  wire [15:0] x7;
  // Hdmi/I2C/BitMaster.hs:76:1-10
  wire  x6;
  // Hdmi/I2C/BitMaster.hs:76:1-10
  wire  x5;
  // Hdmi/I2C/BitMaster.hs:76:1-10
  wire  x4;
  // Hdmi/I2C/BitMaster.hs:76:1-10
  wire  x3;
  // Hdmi/I2C/BitMaster.hs:76:1-10
  wire [32:0] x1;
  // Hdmi/I2C/BitMaster.hs:76:1-10
  wire [9:0] a1;
  wire [9:0] c$bitStateMachineOut;
  // Hdmi/I2C/BitMaster.hs:76:1-10
  wire  din_0;
  wire [62:0] c$bitMasterT_$jOut_app_arg;
  wire [62:0] c$bitMasterT_$jOut_case_alt;
  // Hdmi/I2C/BitMaster.hs:76:1-10
  wire  karg;
  // Hdmi/I2C/BitMaster.hs:76:1-10
  wire [32:0] a1_0;
  wire [32:0] c$busStatusCtrlOut;
  // Hdmi/I2C/BitMaster.hs:76:1-10
  reg [62:0] s = {{{1'b1,   1'b1},   {1'b1,   1'b1},   1'b0,   {{1'b1,   1'b1},   {1'b1,
                                                                  1'b1}},
  {{1'b1,   1'b1},   {1'b1,   1'b1},   {1'b1,   1'b1}},   14'd0,   1'b0,
  1'b0,   1'b0,   1'b0},   {1'b1,   1'b1,   1'b0,   1'b0,   {3'b000,3'bxxx}},
 1'b1,   1'b0,   1'b1,   1'b0,   16'd0};
  // Hdmi/I2C/BitMaster.hs:76:1-10
  wire  sSCL;
  // Hdmi/I2C/BitMaster.hs:76:1-10
  wire  dSCL;
  // Hdmi/I2C/BitMaster.hs:76:1-10
  wire [1:0] i2cI_0;
  // Hdmi/I2C/BitMaster.hs:76:1-10
  wire [2:0] cmd_0;
  // Hdmi/I2C/BitMaster.hs:76:1-10
  wire [15:0] clkCnt_0;
  // Hdmi/I2C/BitMaster.hs:76:1-10
  wire  ena_0;
  // Hdmi/I2C/BitMaster.hs:76:1-10
  wire  rst_0;
  wire [23:0] c$arg;
  wire  c$bitStateMachineOut_fun_arg;
  wire  c$bitStateMachineOut_fun_arg_0;
  wire [9:0] c$bitStateMachineOut_fun_arg_1;
  wire  c$busStatusCtrlOut_fun_arg;
  wire [5:0] c$busStatusCtrlOut_fun_arg_0;
  wire  c$busStatusCtrlOut_fun_arg_1;
  wire  c$busStatusCtrlOut_fun_arg_2;
  wire [32:0] c$busStatusCtrlOut_fun_arg_3;
  wire [7:0] result;
  wire [2:0] result_0;

  assign c$arg = {rst,   ena,   clkCnt,   {cmd,
                                           din},   i2cI};

  assign result = {{s[26:26],   s[58:58],
                    s[19:19]},   s[31:31],   {1'b0,   s[29:29],
                                              1'b0,   s[28:28]}};

  assign result_1 = ((sSCL == (1'b1)) & (dSCL == (1'b0))) ? {x1,
                                                             a1,   s[61:61],   x4,   x5,   x6,   x7} : {x1,
                                                                                                        a1,   x3,   x4,
                                                                                                        x5,   x6,   x7};

  assign x7 = c$bitMasterT_$jOut_app_arg[15:0];

  assign x6 = c$bitMasterT_$jOut_app_arg[16:16];

  assign x5 = c$bitMasterT_$jOut_app_arg[17:17];

  assign x4 = c$bitMasterT_$jOut_app_arg[18:18];

  assign x3 = c$bitMasterT_$jOut_app_arg[19:19];

  assign x1 = c$bitMasterT_$jOut_app_arg[62:30];

  assign a1 = c$bitStateMachineOut;

  assign c$bitStateMachineOut_fun_arg = s[58:58];

  assign c$bitStateMachineOut_fun_arg_0 = s[17:17];

  assign c$bitStateMachineOut_fun_arg_1 = c$bitMasterT_$jOut_app_arg[29:20];

  Hdmi_I2C_BitMaster_bitMaster_bitStateMachine Hdmi_I2C_BitMaster_bitMaster_bitStateMachine_c$bitStateMachineOut
    ( .c$case_alt (c$bitStateMachineOut)
    , .c$arg (rst_0)
    , .c$arg_0 (c$bitStateMachineOut_fun_arg)
    , .c$arg_1 (c$bitStateMachineOut_fun_arg_0)
    , .c$arg_2 (cmd_0)
    , .c$arg_3 (din_0)
    , .c$arg_4 (c$bitStateMachineOut_fun_arg_1) );

  assign din_0 = c$arg[2:2];

  assign c$bitMasterT_$jOut_app_arg = (rst_0 | ((s[15:0] == 16'd0) | ((~ ena_0) | ((dSCL == (1'b1)) & ((sSCL == (1'b0)) & s[29:29]))))) ? {a1_0,
                                                                                                                                           s[29:20],
                                                                                                                                           s[19:19],
                                                                                                                                           s[29:29],
                                                                                                                                           1'b1,
                                                                                                                                           karg,
                                                                                                                                           clkCnt_0} : c$bitMasterT_$jOut_case_alt;

  assign c$bitMasterT_$jOut_case_alt = s[16:16] ? {a1_0,
                                                   s[29:20],   s[19:19],   s[29:29],   1'b0,
                                                   karg,   s[15:0]} : {a1_0,   s[29:20],
                                                                       s[19:19],   s[29:29],   1'b0,   karg,
                                                                       s[15:0] - 16'd1};

  assign karg = ((s[29:29] & (~ s[18:18])) | s[16:16]) & (sSCL == 1'b0);

  assign a1_0 = c$busStatusCtrlOut;

  assign c$busStatusCtrlOut_fun_arg = s[17:17];

  assign c$busStatusCtrlOut_fun_arg_0 = s[25:20];

  assign c$busStatusCtrlOut_fun_arg_1 = s[27:27];

  assign c$busStatusCtrlOut_fun_arg_2 = s[28:28];

  assign c$busStatusCtrlOut_fun_arg_3 = s[62:30];

  Hdmi_I2C_BitMaster_bitMaster_busStatusCtrl Hdmi_I2C_BitMaster_bitMaster_busStatusCtrl_c$busStatusCtrlOut
    ( .result (c$busStatusCtrlOut)
    , .c$arg (rst_0)
    , .c$arg_0 (ena_0)
    , .c$arg_1 (clkCnt_0)
    , .c$arg_2 (cmd_0)
    , .c$arg_3 (c$busStatusCtrlOut_fun_arg)
    , .c$arg_4 (i2cI_0)
    , .c$arg_5 (c$busStatusCtrlOut_fun_arg_0)
    , .c$arg_6 (c$busStatusCtrlOut_fun_arg_1)
    , .c$arg_7 (c$busStatusCtrlOut_fun_arg_2)
    , .c$arg_8 (c$busStatusCtrlOut_fun_arg_3) );

  // register begin
  always @(posedge clk or  posedge  arst) begin : s_register
    if ( arst) begin
      s <= {{{1'b1,   1'b1},   {1'b1,   1'b1},   1'b0,   {{1'b1,   1'b1},   {1'b1,
                                                                    1'b1}},
    {{1'b1,   1'b1},   {1'b1,   1'b1},   {1'b1,   1'b1}},   14'd0,   1'b0,
    1'b0,   1'b0,   1'b0},   {1'b1,   1'b1,   1'b0,   1'b0,   {3'b000,3'bxxx}},
   1'b1,   1'b0,   1'b1,   1'b0,   16'd0};
    end else if (gen) begin
      s <= result_1;
    end
  end
  // register end

  assign sSCL = s[62:62];

  assign dSCL = s[60:60];

  assign i2cI_0 = c$arg[1:0];

  assign cmd_0 = c$arg[5:3];

  assign clkCnt_0 = c$arg[21:6];

  assign ena_0 = c$arg[22:22];

  assign rst_0 = c$arg[23:23];

  assign result_0 = result[7:5];

  assign busy = result[4:4];

  assign i2cO = result[3:0];

  assign cmdAck = result_0[2:2];

  assign al = result_0[1:1];

  assign dout = result_0[0:0];


endmodule
