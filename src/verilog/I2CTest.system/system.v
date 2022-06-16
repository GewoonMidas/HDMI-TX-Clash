/* AUTOMATICALLY GENERATED VERILOG-2001 SOURCE CODE.
** GENERATED BY CLASH 1.6.3. DO NOT MODIFY.
*/
`timescale 100fs/100fs
module system
    ( // No inputs

      // Outputs
      output wire [793:0] result
    );
  wire  c$app_arg;
  wire  c$app_arg_0;

  // resetGen begin
  // pragma translate_off
  reg  rst;
  localparam reset_period = 100000 - 10 + (1 * 100000);
  `ifndef VERILATOR
  initial begin
    #1 rst =  1 ;
    #reset_period rst =  0 ;
  end
  `else
  always begin
    // The redundant (rst | ~ rst) is needed to ensure that this is
    // calculated in every cycle by verilator. Without it, the reset will stop
    // being updated and will be stuck as asserted forever.
    rst = $c("this->reset_gen(",reset_period,",true)") & (rst | ~ rst);
  end
  `systemc_interface
  CData reset_gen(vluint32_t reset_period, bool active_high) {
    static vluint32_t to_wait = reset_period;
    static CData reset = active_high ? 1 : 0;
    static bool finished = false;

    if(!finished) {
      if(to_wait == 0) {
        reset = reset == 0 ? 1 : 0;
        finished = true;
      }
      else {
        to_wait = to_wait - 1;
      }
    }

    return reset;
  }
  `verilog
  `endif
  assign c$app_arg = rst;
  // pragma translate_on
  // resetGen end

  // clockGen begin
  // pragma translate_off
  reg  clk;
  // 1 = 0.1ps
  localparam half_period = (100000 / 2);
  always begin
    // Delay of 1 mitigates race conditions (https://github.com/steveicarus/iverilog/issues/160)
    #1 clk =  0 ;
    `ifndef VERILATOR
    #100000 forever begin
      clk = ~ clk;
      #half_period;
      clk = ~ clk;
      #half_period;
    end
    `else
    clk = $c("this->tb_clock_gen(",half_period,",true)");
    `endif
  end

  `ifdef VERILATOR
    `systemc_interface
    CData tb_clock_gen(vluint32_t half_period, bool active_rising) {
      static vluint32_t init_wait = 100000;
      static vluint32_t to_wait = 0;
      static CData clock = active_rising ? 0 : 1;

      if(init_wait == 0) {
        if(to_wait == 0) {
          to_wait = half_period - 1;
          clock = clock == 0 ? 1 : 0;
        }
        else {
          to_wait = to_wait - 1;
        }
      }
      else {
        init_wait = init_wait - 1;
      }

      return clock;
    }
    `verilog
  `endif

  assign c$app_arg_0 = clk;
  // pragma translate_on
  // clockGen end

  I2CTest_system_system0 I2CTest_system_system0_result
    ( .result (result)
    , .clk (c$app_arg_0)
    , .arst (c$app_arg) );


endmodule
