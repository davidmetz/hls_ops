package hls_float

import circt.stage.ChiselStage
import circt.stage.FirtoolOption
import chisel3.{RawModule, _}
import chisel3.util._
import chisel3.reflect._
import float32._
import float32.Float32._
import float64._
import float64.Float64._

import java.io.PrintWriter

class Handshake[T <: Data](data_type: T) extends Bundle {
  val ready = Output(Bool())
  val valid = Input(Bool())
  val data = Input(data_type)

  def fire(): Bool = {
    ready && valid
  }

  def readyValid: ReadyValidIO[T] = {
    val isFlipped = DataMirror.directionOf(ready) == chisel3.ActualDirection.Input
    val dec = Wire(if (isFlipped) Flipped(Decoupled(data_type)) else Decoupled(data_type))
    dec.ready <> ready
    dec.valid <> valid
    dec.bits <> data
    dec
  }
}

class HandshakeSingleDelay(width: Int) extends Module {
  val i0 = IO(new Handshake(UInt(width.W)))
  val o0 = IO(Flipped(new Handshake(UInt(width.W))))
  val valid = RegInit(false.B)
  val data = Reg(UInt(width.W))
  i0.ready := !valid || o0.ready
  o0.valid := valid
  o0.data := data
  when(o0.fire()) {
    valid := false.B
  }
  when(i0.fire()) {
    valid := true.B
    data := i0.data
  }
}

class op_SITOFP_I32W_O32W extends RawModule {
  val i0 = IO(new Handshake(SInt(32.W)))
  val o0 = IO(Flipped(new Handshake(UInt(32.W))))
  val clk = IO(Input(Clock()))
  val reset = IO(Input(Bool()))
  withClockAndReset(clk, reset) {
    o0.valid := i0.valid
    i0.ready := o0.ready
    o0.data := i0.data.toFloat32().asUInt
  }
}

class op_SIToFP_I32W_O32W extends RawModule {
  val i0 = IO(new Handshake(SInt(32.W)))
  val o0 = IO(Flipped(new Handshake(UInt(32.W))))
  val clk = IO(Input(Clock()))
  val reset = IO(Input(Bool()))
  withClockAndReset(clk, reset) {
    o0.valid := i0.valid
    i0.ready := o0.ready
    o0.data := i0.data.toFloat32().asUInt
  }
}

class op_UITOFP_I32W_O32W extends RawModule {
  val i0 = IO(new Handshake(UInt(32.W)))
  val o0 = IO(Flipped(new Handshake(UInt(32.W))))
  val clk = IO(Input(Clock()))
  val reset = IO(Input(Bool()))
  withClockAndReset(clk, reset) {
    o0.valid := i0.valid
    i0.ready := o0.ready
    o0.data := i0.data.toFloat32().asUInt
  }
}

class op_FP2UI_I32W_O32W extends RawModule {
  val i0 = IO(new Handshake(SInt(32.W)))
  val o0 = IO(Flipped(new Handshake(UInt(32.W))))
  val clk = IO(Input(Clock()))
  val reset = IO(Input(Bool()))
  withClockAndReset(clk, reset) {
    o0.valid := i0.valid
    i0.ready := o0.ready
    o0.data := i0.data.asTypeOf(new Float32).toUInt()
  }
}

class op_FP2SI_I32W_O32W extends RawModule {
  val i0 = IO(new Handshake(UInt(32.W)))
  val o0 = IO(Flipped(new Handshake(SInt(32.W))))
  val clk = IO(Input(Clock()))
  val reset = IO(Input(Bool()))
  withClockAndReset(clk, reset) {
    o0.valid := i0.valid
    i0.ready := o0.ready
    o0.data := i0.data.asTypeOf(new Float32).toSInt()
  }
}

class op_FpToSInt_I32W_O32W extends RawModule {
  val i0 = IO(new Handshake(UInt(32.W)))
  val o0 = IO(Flipped(new Handshake(SInt(32.W))))
  val clk = IO(Input(Clock()))
  val reset = IO(Input(Bool()))
  withClockAndReset(clk, reset) {
    o0.valid := i0.valid
    i0.ready := o0.ready
    o0.data := i0.data.asTypeOf(new Float32).toSInt()
  }
}

class op_FPOP_sub_I32W_I32W_O32W extends RawModule {
  val i0 = IO(new Handshake(UInt(32.W)))
  val i1 = IO(new Handshake(UInt(32.W)))
  val o0 = IO(Flipped(new Handshake(UInt(32.W))))
  val clk = IO(Input(Clock()))
  val reset = IO(Input(Bool()))


  val neg = WireInit(i1.data.asTypeOf(new Float32).neg()).asUInt
  dontTouch(neg)
  val add = Module(new op_FPOP_add_I32W_I32W_O32W())
  add.clk := clk
  add.reset := reset
  add.i0 <> i0
  add.i1 <> i1
  add.i1.data := neg
  add.o0 <> o0
}

//class op_FPOP_add_I32W_I32W_O32W extends RawModule {
//    val i0 = IO(new Handshake(UInt(32.W)))
//    val i1 = IO(new Handshake(UInt(32.W)))
//    val o0 = IO(Flipped(new Handshake(UInt(32.W))))
//    val clk  = IO(Input(Clock()))
//    val reset = IO(Input(Bool()))
//    withClockAndReset(clk, reset) {
//        o0.valid := i0.valid && i1.valid
//        i0.ready := o0.ready && o0.fire()
//        i1.ready := o0.ready && o0.fire()
//        o0.data := (i0.data.asTypeOf(new Float32) + i1.data.asTypeOf(new Float32)).asUInt
//    }
//}
//
//class op_FPOP_mul_I32W_I32W_O32W extends RawModule {
//    val i0 = IO(new Handshake(UInt(32.W)))
//    val i1 = IO(new Handshake(UInt(32.W)))
//    val o0 = IO(Flipped(new Handshake(UInt(32.W))))
//    val clk  = IO(Input(Clock()))
//    val reset = IO(Input(Bool()))
//    withClockAndReset(clk, reset) {
//        o0.valid := i0.valid && i1.valid
//        i0.ready := o0.ready && o0.fire()
//        i1.ready := o0.ready && o0.fire()
//        o0.data := (i0.data.asTypeOf(new Float32) * i1.data.asTypeOf(new Float32)).asUInt
//    }
//}

class vivado_FADD_test extends RawModule {
  val i0 = IO(new Handshake(UInt(32.W)))
  val i1 = IO(new Handshake(UInt(32.W)))
  val o0 = IO(Flipped(new Handshake(UInt(32.W))))
  val clk = IO(Input(Clock()))
  val reset = IO(Input(Bool()))

  val mod = Module(new vivado_fadd_blocking).io

  // Connect clock and enable
  mod.aclken := true.B
  mod.aclk := clk
  mod.aresetn := !reset

  // Connect input handshakes (i0, i1) to mod's input AXI-Stream ports
  mod.s_axis_a_tvalid := i0.valid
  mod.s_axis_a_tdata := i0.data
  i0.ready := mod.s_axis_a_tready

  mod.s_axis_b_tvalid := i1.valid
  mod.s_axis_b_tdata := i1.data
  i1.ready := mod.s_axis_b_tready

  // Connect output handshakes (o0) to mod's output AXI-Stream ports
  o0.valid := mod.m_axis_result_tvalid
  o0.data := mod.m_axis_result_tdata
  mod.m_axis_result_tready := o0.ready
}

class op_FPOP_add_I32W_I32W_O32W extends RawModule {
  val i0 = IO(new Handshake(UInt(32.W)))
  val i1 = IO(new Handshake(UInt(32.W)))
  val o0 = IO(Flipped(new Handshake(UInt(32.W))))
  val clk = IO(Input(Clock()))
  val reset = IO(Input(Bool()))

  withModulePrefix(desiredName) {
    withClockAndReset(clock = clk, reset = reset) {
      val mod = Module(new vivado_fadd_comb).io
      val delay = Module(new HandshakeSingleDelay(32))

      // Connect input handshakes (i0, i1) to mod's input AXI-Stream ports
      mod.s_axis_a_tvalid := i0.valid
      mod.s_axis_a_tdata := i0.data
      i0.ready := delay.i0.fire()

      mod.s_axis_b_tvalid := i1.valid
      mod.s_axis_b_tdata := i1.data
      i1.ready := delay.i0.fire()

      // Connect output handshakes (o0) to mod's output AXI-Stream ports
      delay.i0.valid := mod.m_axis_result_tvalid
      o0.valid := delay.o0.valid
      delay.i0.data := mod.m_axis_result_tdata
      o0.data := delay.o0.data
      delay.o0.ready := o0.ready
    }
  }
}

class op_FPOP_mul_I32W_I32W_O32W extends RawModule {
  val i0 = IO(new Handshake(UInt(32.W)))
  val i1 = IO(new Handshake(UInt(32.W)))
  val o0 = IO(Flipped(new Handshake(UInt(32.W))))
  val clk = IO(Input(Clock()))
  val reset = IO(Input(Bool()))

  withModulePrefix(desiredName) {
    withClockAndReset(clock = clk, reset = reset) {
      val mod = Module(new vivado_fmul_comb).io
      val delay = Module(new HandshakeSingleDelay(32))

      // Connect input handshakes (i0, i1) to mod's input AXI-Stream ports
      mod.s_axis_a_tvalid := i0.valid
      mod.s_axis_a_tdata := i0.data
      i0.ready := delay.i0.fire()

      mod.s_axis_b_tvalid := i1.valid
      mod.s_axis_b_tdata := i1.data
      i1.ready := delay.i0.fire()

      // Connect output handshakes (o0) to mod's output AXI-Stream ports
      delay.i0.valid := mod.m_axis_result_tvalid
      o0.valid := delay.o0.valid
      delay.i0.data := mod.m_axis_result_tdata
      o0.data := delay.o0.data
      delay.o0.ready := o0.ready
    }
  }
}

class op_FP_0_0E_0__O32W extends RawModule {
  val o0 = IO(Flipped(new Handshake(UInt(32.W))))
  val clk = IO(Input(Clock()))
  val reset = IO(Input(Bool()))
  withClockAndReset(clk, reset) {
    o0.valid := true.B
    o0.data := 0.U.toFloat32().asUInt
  }
}

class op_FP_1_0E_0__O32W extends RawModule {
  val o0 = IO(Flipped(new Handshake(UInt(32.W))))
  val clk = IO(Input(Clock()))
  val reset = IO(Input(Bool()))
  withClockAndReset(clk, reset) {
    o0.valid := true.B
    o0.data := 1.U.toFloat32().asUInt
  }
}

class op_FP_2_0E_0__O32W extends RawModule {
  val o0 = IO(Flipped(new Handshake(UInt(32.W))))
  val clk = IO(Input(Clock()))
  val reset = IO(Input(Bool()))
  withClockAndReset(clk, reset) {
    o0.valid := true.B
    o0.data := 2.U.toFloat32().asUInt
  }
}

class vivado_fadd_comb extends BlackBox {
  val io = IO(new Bundle {
    val s_axis_a_tvalid = Input(Bool())
    val s_axis_a_tdata = Input(UInt(32.W))
    val s_axis_b_tvalid = Input(Bool())
    val s_axis_b_tdata = Input(UInt(32.W))
    val m_axis_result_tvalid = Output(Bool())
    val m_axis_result_tdata = Output(UInt(32.W))
  })
}

class vivado_fmul_comb extends BlackBox {
  val io = IO(new Bundle {
    val s_axis_a_tvalid = Input(Bool())
    val s_axis_a_tdata = Input(UInt(32.W))
    val s_axis_b_tvalid = Input(Bool())
    val s_axis_b_tdata = Input(UInt(32.W))
    val m_axis_result_tvalid = Output(Bool())
    val m_axis_result_tdata = Output(UInt(32.W))
  })
}

class vivado_fadd_blocking extends BlackBox {
  val io = IO(new Bundle {
    val aclk = Input(Clock())
    val aclken = Input(Bool())
    val aresetn = Input(Bool())
    val s_axis_a_tvalid = Input(Bool())
    val s_axis_a_tready = Output(Bool())
    val s_axis_a_tdata = Input(UInt(32.W))
    val s_axis_b_tvalid = Input(Bool())
    val s_axis_b_tready = Output(Bool())
    val s_axis_b_tdata = Input(UInt(32.W))
    val m_axis_result_tvalid = Output(Bool())
    val m_axis_result_tready = Input(Bool())
    val m_axis_result_tdata = Output(UInt(32.W))
  })
  new PrintWriter("vivado_fadd_blocking.tcl") {
    write(
      """# BEGIN Vivado Commands
        |set vivado_ver [version -short]
        |set fpo_ver 7.1
        |if {[regexp -nocase {2015\.1.*} $vivado_ver match]} {
        |    set fpo_ver 7.0
        |}
        |create_ip -name floating_point -version $fpo_ver -vendor xilinx.com -library ip -module_name vivado_fadd_blocking
        |# BEGIN Vivado Commands
        |# BEGIN Vivado Parameters
        |set_property -dict [list CONFIG.a_precision_type Single \
        |                          CONFIG.a_tuser_width 1 \
        |                          CONFIG.add_sub_value Add \
        |                          CONFIG.b_tuser_width 1 \
        |                          CONFIG.c_a_exponent_width 8 \
        |                          CONFIG.c_a_fraction_width 24 \
        |                          CONFIG.c_compare_operation Programmable \
        |                          CONFIG.c_has_divide_by_zero false \
        |                          CONFIG.c_has_invalid_op false \
        |                          CONFIG.c_has_overflow false \
        |                          CONFIG.c_has_underflow false \
        |                          CONFIG.c_latency 1 \
        |                          CONFIG.c_mult_usage Full_Usage \
        |                          CONFIG.c_optimization Speed_Optimized \
        |                          CONFIG.c_rate 1 \
        |                          CONFIG.c_result_exponent_width 8 \
        |                          CONFIG.c_result_fraction_width 24 \
        |                          CONFIG.component_name vivado_fadd_blocking \
        |                          CONFIG.flow_control Blocking \
        |                          CONFIG.has_a_tlast false \
        |                          CONFIG.has_a_tuser false \
        |                          CONFIG.has_aclken true \
        |                          CONFIG.has_aresetn true \
        |                          CONFIG.has_b_tlast false \
        |                          CONFIG.has_b_tuser false \
        |                          CONFIG.has_operation_tlast false \
        |                          CONFIG.has_operation_tuser false \
        |                          CONFIG.has_result_tready true \
        |                          CONFIG.maximum_latency false \
        |                          CONFIG.operation_tuser_width 1 \
        |                          CONFIG.operation_type Add_Subtract \
        |                          CONFIG.result_precision_type Single \
        |                          CONFIG.result_tlast_behv Null] -objects [get_ips vivado_fadd_blocking] -quiet
        |# END Vivado Parameters
        |set_property generate_synth_checkpoint false [get_files vivado_fadd_blocking.xci]
        |generate_target {synthesis simulation} [get_files vivado_fadd_blocking.xci]
        |""".stripMargin);
    close()
  }
}

class vivado_fmul_blocking extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val s_axis_a_tvalid = Input(Bool())
    val s_axis_a_tready = Output(Bool())
    val s_axis_a_tdata = Input(UInt(32.W))
    val s_axis_b_tvalid = Input(Bool())
    val s_axis_b_tready = Output(Bool())
    val s_axis_b_tdata = Input(UInt(32.W))
    val m_axis_result_tvalid = Output(Bool())
    val m_axis_result_tready = Input(Bool())
    val m_axis_result_tdata = Output(UInt(32.W))
  })
  new PrintWriter("vivado_fmul_blocking.tcl") {
    write(
      """# BEGIN Vivado Commands
        |set vivado_ver [version -short]
        |set fpo_ver 7.1
        |if {[regexp -nocase {2015\.1.*} $vivado_ver match]} {
        |    set fpo_ver 7.0
        |}
        |create_ip -name floating_point -version $fpo_ver -vendor xilinx.com -library ip -module_name vivado_fmul_blocking
        |# BEGIN Vivado Commands
        |# BEGIN Vivado Parameters
        |set_property -dict [list CONFIG.a_precision_type Single \
        |                          CONFIG.a_tuser_width 1 \
        |                          CONFIG.add_sub_value Both \
        |                          CONFIG.b_tuser_width 1 \
        |                          CONFIG.c_a_exponent_width 8 \
        |                          CONFIG.c_a_fraction_width 24 \
        |                          CONFIG.c_compare_operation Programmable \
        |                          CONFIG.c_has_divide_by_zero false \
        |                          CONFIG.c_has_invalid_op false \
        |                          CONFIG.c_has_overflow false \
        |                          CONFIG.c_has_underflow false \
        |                          CONFIG.c_latency 0 \
        |                          CONFIG.c_mult_usage Max_Usage \
        |                          CONFIG.c_optimization Speed_Optimized \
        |                          CONFIG.c_rate 1 \
        |                          CONFIG.c_result_exponent_width 8 \
        |                          CONFIG.c_result_fraction_width 24 \
        |                          CONFIG.component_name vivado_fmul_blocking \
        |                          CONFIG.flow_control Blocking \
        |                          CONFIG.has_a_tlast false \
        |                          CONFIG.has_a_tuser false \
        |                          CONFIG.has_aclken false \
        |                          CONFIG.has_aresetn false \
        |                          CONFIG.has_b_tlast false \
        |                          CONFIG.has_b_tuser false \
        |                          CONFIG.has_operation_tlast false \
        |                          CONFIG.has_operation_tuser false \
        |                          CONFIG.has_result_tready true \
        |                          CONFIG.maximum_latency false \
        |                          CONFIG.operation_tuser_width 1 \
        |                          CONFIG.operation_type Multiply \
        |                          CONFIG.result_precision_type Single \
        |                          CONFIG.result_tlast_behv Null] -objects [get_ips vivado_fmul_blocking] -quiet
        |# END Vivado Parameters
        |set_property generate_synth_checkpoint false [get_files vivado_fmul_blocking.xci]
        |generate_target {synthesis simulation} [get_files vivado_fmul_blocking.xci]
        |""".stripMargin);
    close()
  }
}

class float_add_dpi extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val a = Input(UInt(32.W))
    val b = Input(UInt(32.W))
    val out = Output(UInt(32.W))
  })

  // Inline Verilog code to call the DPI-C function
  setInline("float_add_dpi.v",
    """|module float_add_dpi (
       |  input  [31:0] a,
       |  input  [31:0] b,
       |  output [31:0] out
       |);
       |  import "DPI-C" function void float_add_dpi_c(input bit [31:0] a, input bit [31:0] b, output bit [31:0] out);
       |
       |  // Call the DPI-C function
       |  always_comb begin
       |    float_add_dpi_c(a, b, out);
       |  end
       |endmodule
       |""".stripMargin)
}

class float_mul_dpi extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val a = Input(UInt(32.W))
    val b = Input(UInt(32.W))
    val out = Output(UInt(32.W))
  })

  // Inline Verilog code to call the DPI-C function
  setInline("float_mul_dpi.v",
    """|module float_mul_dpi (
       |  input  [31:0] a,
       |  input  [31:0] b,
       |  output [31:0] out
       |);
       |  import "DPI-C" function void float_mul_dpi_c(input bit [31:0] a, input bit [31:0] b, output bit [31:0] out);
       |
       |  // Call the DPI-C function
       |  always_comb begin
       |    float_mul_dpi_c(a, b, out);
       |  end
       |endmodule
       |""".stripMargin)
}

class vivado_fadd_blocking_dummy extends RawModule {
  override def desiredName: String = "vivado_fadd_blocking"

  val aclk = IO(Input(Clock()))
  val aclken = IO(Input(Bool()))
  val aresetn = IO(Input(Bool()))
  val s_axis_a_tvalid = IO(Input(Bool()))
  val s_axis_a_tready = IO(Output(Bool()))
  val s_axis_a_tdata = IO(Input(UInt(32.W)))
  val s_axis_b_tvalid = IO(Input(Bool()))
  val s_axis_b_tready = IO(Output(Bool()))
  val s_axis_b_tdata = IO(Input(UInt(32.W)))
  val m_axis_result_tvalid = IO(Output(Bool()))
  val m_axis_result_tready = IO(Input(Bool()))
  val m_axis_result_tdata = IO(Output(UInt(32.W)))
  withClockAndReset(aclk, !aresetn) {
    val delay = Module(new HandshakeSingleDelay(32))
    val add = Module(new float_add_dpi)
    delay.i0.valid := s_axis_a_tvalid && s_axis_b_tvalid
    add.io.a := s_axis_a_tdata
    add.io.b := s_axis_b_tdata
    //        delay.i0.data := (s_axis_a_tdata.asTypeOf(new Float32) + s_axis_b_tdata.asTypeOf(new Float32)).asUInt
    delay.i0.data := add.io.out
    s_axis_a_tready := delay.i0.ready && delay.i0.valid
    s_axis_b_tready := delay.i0.ready && delay.i0.valid
    m_axis_result_tvalid := delay.o0.valid
    m_axis_result_tdata := delay.o0.data
    delay.o0.ready := m_axis_result_tready
  }
}

class vivado_fmul_blocking_dummy extends RawModule {
  override def desiredName: String = "vivado_fmul_blocking"

  val s_axis_a_tvalid = IO(Input(Bool()))
  val s_axis_a_tready = IO(Output(Bool()))
  val s_axis_a_tdata = IO(Input(UInt(32.W)))
  val s_axis_b_tvalid = IO(Input(Bool()))
  val s_axis_b_tready = IO(Output(Bool()))
  val s_axis_b_tdata = IO(Input(UInt(32.W)))
  val m_axis_result_tvalid = IO(Output(Bool()))
  val m_axis_result_tready = IO(Input(Bool()))
  val m_axis_result_tdata = IO(Output(UInt(32.W)))
  // combinatorial multiplication
  m_axis_result_tvalid := (s_axis_a_tvalid && s_axis_b_tvalid)
  s_axis_a_tready := m_axis_result_tready && m_axis_result_tvalid
  s_axis_b_tready := m_axis_result_tready && m_axis_result_tvalid
  val mul = Module(new float_mul_dpi)
  mul.io.a := s_axis_a_tdata
  mul.io.b := s_axis_b_tdata
  //    m_axis_result_tdata := ((s_axis_a_tdata.asTypeOf(new Float32) * s_axis_b_tdata.asTypeOf(new Float32)).asUInt)
  m_axis_result_tdata := mul.io.out
}

class spmv_kernel_fadd_32ns_32ns_32_3_full_dsp_1_ip extends RawModule {
  val aclk = IO(Input(Clock()))
  val aclken = IO(Input(Bool()))
  val s_axis_a_tvalid = IO(Input(Bool()))
  val s_axis_a_tdata = IO(Input(UInt(32.W)))
  val s_axis_b_tvalid = IO(Input(Bool()))
  val s_axis_b_tdata = IO(Input(UInt(32.W)))
  val m_axis_result_tvalid = IO(Output(Bool()))
  val m_axis_result_tdata = IO(Output(UInt(32.W)))
  withClock(aclk) {
    m_axis_result_tvalid := RegNext(s_axis_a_tvalid && s_axis_b_tvalid)
    m_axis_result_tdata := RegNext((s_axis_a_tdata.asTypeOf(new Float32) + s_axis_b_tdata.asTypeOf(new Float32)).asUInt)
  }
}

class spmv_kernel_fadd_32ns_32ns_32_4_full_dsp_1_ip extends RawModule {
  val aclk = IO(Input(Clock()))
  val aclken = IO(Input(Bool()))
  val s_axis_a_tvalid = IO(Input(Bool()))
  val s_axis_a_tdata = IO(Input(UInt(32.W)))
  val s_axis_b_tvalid = IO(Input(Bool()))
  val s_axis_b_tdata = IO(Input(UInt(32.W)))
  val m_axis_result_tvalid = IO(Output(Bool()))
  val m_axis_result_tdata = IO(Output(UInt(32.W)))
  withClock(aclk) {
    m_axis_result_tvalid := RegNext(RegNext(s_axis_a_tvalid && s_axis_b_tvalid))
    m_axis_result_tdata := RegNext(RegNext((s_axis_a_tdata.asTypeOf(new Float32) + s_axis_b_tdata.asTypeOf(new Float32)).asUInt))
  }
}

class spmv_kernel_fmul_32ns_32ns_32_2_max_dsp_1_ip extends RawModule {

  val s_axis_a_tvalid = IO(Input(Bool()))
  val s_axis_a_tdata = IO(Input(UInt(32.W)))
  val s_axis_b_tvalid = IO(Input(Bool()))
  val s_axis_b_tdata = IO(Input(UInt(32.W)))
  val m_axis_result_tvalid = IO(Output(Bool()))
  val m_axis_result_tdata = IO(Output(UInt(32.W)))
  m_axis_result_tvalid := (s_axis_a_tvalid && s_axis_b_tvalid)
  m_axis_result_tdata := ((s_axis_a_tdata.asTypeOf(new Float32) * s_axis_b_tdata.asTypeOf(new Float32)).asUInt)
}

class spmv_kernel_fmul_32ns_32ns_32_3_max_dsp_1_ip extends RawModule {
  val aclk = IO(Input(Clock()))
  val aclken = IO(Input(Bool()))
  val s_axis_a_tvalid = IO(Input(Bool()))
  val s_axis_a_tdata = IO(Input(UInt(32.W)))
  val s_axis_b_tvalid = IO(Input(Bool()))
  val s_axis_b_tdata = IO(Input(UInt(32.W)))
  val m_axis_result_tvalid = IO(Output(Bool()))
  val m_axis_result_tdata = IO(Output(UInt(32.W)))
  withClock(aclk) {
    m_axis_result_tvalid := RegNext(s_axis_a_tvalid && s_axis_b_tvalid)
    m_axis_result_tdata := RegNext((s_axis_a_tdata.asTypeOf(new Float32) * s_axis_b_tdata.asTypeOf(new Float32)).asUInt)
  }
}

class RhlsBuf(val depth: Int, val width: Int, val pass_through: Boolean) extends RawModule {
  override def desiredName: String = s"op_HLS_BUF${if (pass_through) "_P" else ""}_${depth}_I${width}W_O${width}W"

  val i0 = IO(new Handshake(UInt(width.W)))
  val o0 = IO(Flipped(new Handshake(UInt(width.W))))
  val clk = IO(Input(Clock()))
  val reset = IO(Input(Bool()))
  withModulePrefix(desiredName) {
    withClockAndReset(clk, reset) {
      if (depth != 0) {
        val queue = Module(new Queue(UInt(width.W), depth, false, pass_through, true, false))
        queue.io.enq <> i0.readyValid
        when(reset) {
          queue.io.enq.valid := false.B
        }
        queue.io.deq <> o0.readyValid
        dontTouch(queue.io.count)
      } else {
        require(pass_through)
        i0 <> o0
      }
    }
  }
}

object RhlsBuf {
  def from_name(name: String): RhlsBuf = {
    name match {
      case s"op_HLS_BUF_P_${depth}_I${width}W_O${width2}W" => new RhlsBuf(depth.toInt, width.toInt, true)
      case s"op_HLS_BUF_${depth}_I${width}W_O${width2}W" => new RhlsBuf(depth.toInt, width.toInt, false)
    }
  }
}

class RhlsDecLoad(val depth: Int, val width: Int, val addr_width: Int, val type_name: String) extends RawModule {
  //    op_HLS_DEC_LOAD_10_ptr_I64W_I64W_O64W_O64W
  override def desiredName: String = s"op_HLS_DEC_LOAD_${depth}_${type_name}_I${addr_width}W_I${width}W_O${width}W_O${addr_width}W"
  val i0 = IO(new Handshake(UInt(addr_width.W)))
  val i1 = IO(new Handshake(UInt(width.W)))
  val o0 = IO(Flipped(new Handshake(UInt(width.W))))
  val o1 = IO(Flipped(new Handshake(UInt(addr_width.W))))
  val clk = IO(Input(Clock()))
  val reset = IO(Input(Bool()))
  withModulePrefix(desiredName) {
    withClockAndReset(clk, reset) {
      val queue = Module(new Queue(UInt(width.W), depth, false, true, true, false))
      queue.io.enq <> i1.readyValid
      when(reset) {
        queue.io.enq.valid := false.B
      }
      queue.io.deq <> o0.readyValid
      // use a queue without data
      val request_in_flight = Module(new Queue(Bool(), depth, false, true, false, false))
      request_in_flight.io.enq.bits := DontCare
      // have count available in waveforms
      dontTouch(request_in_flight.io.count)
      val can_request = WireInit(request_in_flight.io.enq.ready)
      o1.valid := i0.valid && can_request
      o1.data := i0.data
      i0.ready := o1.ready && can_request
      // new request in flight
      request_in_flight.io.enq.valid := o1.fire()
      // data leaves load
      request_in_flight.io.deq.ready := o0.fire()
    }
  }
}

object RhlsDecLoad {
  def from_name(name: String): RhlsDecLoad = {
    name match {
      case s"op_HLS_DEC_LOAD_${depth}_${type_name}_I${addr_width}W_I${width}W_O${width2}W_O${addr_width2}W" => new RhlsDecLoad(depth.toInt, width.toInt, addr_width.toInt, type_name)
    }
  }
}

/*
 * Enable the generation of the FIRRTL and Verilog equivalents once called via :
 * sbt "runMain fpu.FPU"
 */
object VerilogGenerator extends App {
  //    emitVerilog(new op_SITOFP_I32W_O32W)
  //    emitVerilog(new op_FpToSInt_I32W_O32W)
  //    emitVerilog(new op_UITOFP_I32W_O32W)
  //    emitVerilog(new op_FP2UI_I32W_O32W)
  //    emitVerilog(new op_FP2SI_I32W_O32W)
  //    emitVerilog(new op_FPOP_sub_I32W_I32W_O32W)
//      emitVerilog(new op_FPOP_add_I32W_I32W_O32W)
//      emitVerilog(new op_FPOP_mul_I32W_I32W_O32W)
  //    emitVerilog(new op_FP_0_0E_0__O32W)
  //    emitVerilog(new op_FP_1_0E_0__O32W)
      emitVerilog(new op_FP_2_0E_0__O32W)
  //    emitVerilog(new spmv_kernel_fadd_32ns_32ns_32_3_full_dsp_1_ip)
  //    emitVerilog(new spmv_kernel_fmul_32ns_32ns_32_2_max_dsp_1_ip)
  //    emitVerilog(new spmv_kernel_fadd_32ns_32ns_32_4_full_dsp_1_ip)
  //    emitVerilog(new spmv_kernel_fmul_32ns_32ns_32_3_max_dsp_1_ip)
  //    emitVerilog(new op_HLS_BUF_P_128_I32W_O32W)
  //    emitVerilog(new vivado_fadd_blocking_dummy)
  //    emitVerilog(new vivado_fmul_blocking_dummy)

  //    emitVerilog(new RhlsBuf(128, 64, true))
  //    emitVerilog(new RhlsBuf(128, 64, false))
  //    emitVerilog(new RhlsBuf(128, 32, true))
  //    emitVerilog(new RhlsBuf(128, 32, false))
  //    emitVerilog(new RhlsBuf(128, 1, true))
  //    emitVerilog(new RhlsBuf(128, 1, false))
  //    emitVerilog(new RhlsBuf(100, 1, false))
  //    emitVerilog(new RhlsBuf(101, 1, true))
  //    emitVerilog(new RhlsBuf(128, 1, false))
  //    emitVerilog(new RhlsBuf(128, 1, true))
  //    emitVerilog(new RhlsBuf(128, 32, false))
  //    emitVerilog(new RhlsBuf(128, 32, true))
  //    emitVerilog(new RhlsBuf(2, 1, false))
  //    emitVerilog(new RhlsBuf(2, 1, true))
  //    emitVerilog(new RhlsBuf(2, 32, false))
  //    emitVerilog(new RhlsBuf(2, 32, true))
  //    emitVerilog(new RhlsBuf(2, 64, false))
  //    emitVerilog(new RhlsBuf(2, 64, true))
  //    emitVerilog(new RhlsBuf(23, 1, true))
  //    emitVerilog(new RhlsBuf(3, 1, true))
  //    emitVerilog(new RhlsBuf(3, 32, false))
  //    emitVerilog(new RhlsBuf(4, 32, true))
  //    emitVerilog(new RhlsBuf(41, 32, true))
  //    emitVerilog(new RhlsBuf(52, 32, true))
  //    emitVerilog(new RhlsBuf(64, 1, true))
  //    emitVerilog(new RhlsBuf(65, 1, false))
  //    emitVerilog(new RhlsBuf(65, 1, true))
  //    emitVerilog(new RhlsBuf(65, 64, true))
  //    emitVerilog(new RhlsBuf(94, 1, true))
  //    emitVerilog(new RhlsBuf(94, 32, true))
  //    emitVerilog(new RhlsBuf(99, 1, true))

  //    emitVerilog(new RhlsBuf(128, 1, false))
  //    emitVerilog(new RhlsBuf(128, 1, true))
  //    emitVerilog(new RhlsBuf(128, 32, true))
  //    emitVerilog(new RhlsBuf(128, 64, true))
  //    emitVerilog(new RhlsBuf(16, 1, true))
  //    emitVerilog(new RhlsBuf(16, 32, true))
  //    emitVerilog(new RhlsBuf(2, 1, false))
  //    emitVerilog(new RhlsBuf(2, 1, true))
  //    emitVerilog(new RhlsBuf(2, 32, false))
  //    emitVerilog(new RhlsBuf(2, 32, true))
  //    emitVerilog(new RhlsBuf(2, 64, false))
  //    emitVerilog(new RhlsBuf(2, 64, true))
  //    emitVerilog(new RhlsBuf(4, 1, false))

  //    emitVerilog(new RhlsBuf(256, 64, true))
  //    emitVerilog(new RhlsBuf(256, 64, false))
  //    emitVerilog(new RhlsBuf(256, 32, true))
  //    emitVerilog(new RhlsBuf(256, 32, false))
  //    emitVerilog(new RhlsBuf(256, 1, true))
  //    emitVerilog(new RhlsBuf(256, 1, false))


  //    emitVerilog(new RhlsBuf(10, 1, false))
  //    emitVerilog(new RhlsBuf(10, 1, true))
  //    emitVerilog(new RhlsBuf(10, 2, false))
  //    emitVerilog(new RhlsBuf(10, 2, true))
  //    emitVerilog(new RhlsBuf(10, 8, false))
  //    emitVerilog(new RhlsBuf(10, 8, true))
  //    emitVerilog(new RhlsBuf(10, 16, false))
  //    emitVerilog(new RhlsBuf(10, 16, true))
  //    emitVerilog(new RhlsBuf(10, 32, false))
  //    emitVerilog(new RhlsBuf(10, 32, true))
  //    emitVerilog(new RhlsBuf(10, 64, false))
  //    emitVerilog(new RhlsBuf(10, 64, true))

  //    emitVerilog(new RhlsBuf(2, 1, false))
  //    emitVerilog(new RhlsBuf(2, 1, true))
  //    emitVerilog(new RhlsBuf(2, 2, false))
  //    emitVerilog(new RhlsBuf(2, 2, true))
  //    emitVerilog(new RhlsBuf(2, 8, false))
  //    emitVerilog(new RhlsBuf(2, 8, true))
  //    emitVerilog(new RhlsBuf(2, 16, false))
  //    emitVerilog(new RhlsBuf(2, 16, true))
  //    emitVerilog(new RhlsBuf(2, 32, false))
  //    emitVerilog(new RhlsBuf(2, 32, true))
  //    emitVerilog(new RhlsBuf(2, 64, false))
  //    emitVerilog(new RhlsBuf(2, 64, true))

//  emitVerilog(new RhlsBuf(20, 1, false))
//  emitVerilog(new RhlsBuf(20, 1, true))
//  emitVerilog(new RhlsBuf(20, 2, false))
//  emitVerilog(new RhlsBuf(20, 2, true))
//  emitVerilog(new RhlsBuf(20, 8, false))
//  emitVerilog(new RhlsBuf(20, 8, true))
//  emitVerilog(new RhlsBuf(20, 16, false))
//  emitVerilog(new RhlsBuf(20, 16, true))
//  emitVerilog(new RhlsBuf(20, 32, false))
//  emitVerilog(new RhlsBuf(20, 32, true))
//  emitVerilog(new RhlsBuf(20, 64, false))
//  emitVerilog(new RhlsBuf(20, 64, true))

//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_10_ptr_I64W_I64W_O64W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_10_bit32_I64W_I32W_O32W_O64W"))
//  emitVerilog(RhlsBuf.from_name("op_HLS_BUF_P_20_I512W_O512W"))
//  emitVerilog(RhlsBuf.from_name("op_HLS_BUF_5_I32W_O32W"))
//    emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_10_float_I64W_I32W_O32W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_100_float_I64W_I32W_O32W_O64W"))
//    emitVerilog(RhlsBuf.from_name("op_HLS_BUF_100_I32W_O32W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_100_bit32_I64W_I32W_O32W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_10_fixedvector_bit32_4__I64W_I128W_O128W_O64W"))
//  emitVerilog(RhlsBuf.from_name("op_HLS_BUF_P_10_I128W_O128W"))
  //  emitVerilog(RhlsBuf.from_name("op_HLS_BUF_P_20_I128W_O128W"))
  //  emitVerilog(RhlsBuf.from_name("op_HLS_BUF_2_I128W_O128W"))
//  emitVerilog(RhlsBuf.from_name("op_HLS_BUF_128_I32W_O32W"))
//  emitVerilog(RhlsBuf.from_name("op_HLS_BUF_128_I128W_O128W"))
//    emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_128_fixedvector_bit32_4__I64W_I128W_O128W_O64W"))
//    emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_128_bit32_I64W_I32W_O32W_O64W"))
//    emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_256_ptr_I64W_I64W_O64W_O64W"))

//  emitVerilog(new RhlsBuf(0, 1, true))
//  emitVerilog(new RhlsBuf(0, 2, true))
//  emitVerilog(new RhlsBuf(0, 8, true))
//  emitVerilog(new RhlsBuf(0, 16, true))
//  emitVerilog(new RhlsBuf(0, 32, true))
//  emitVerilog(new RhlsBuf(0, 64, true))
//  emitVerilog(new RhlsBuf(0, 128, true))
//  emitVerilog(new RhlsBuf(1, 1, false))
//  emitVerilog(new RhlsBuf(1, 1, true))
//  emitVerilog(new RhlsBuf(1, 2, false))
//  emitVerilog(new RhlsBuf(1, 2, true))
//  emitVerilog(new RhlsBuf(1, 8, false))
//  emitVerilog(new RhlsBuf(1, 8, true))
//  emitVerilog(new RhlsBuf(1, 16, false))
//  emitVerilog(new RhlsBuf(1, 16, true))
//  emitVerilog(new RhlsBuf(1, 32, false))
//  emitVerilog(new RhlsBuf(1, 32, true))
//  emitVerilog(new RhlsBuf(1, 64, false))
//  emitVerilog(new RhlsBuf(1, 64, true))
//  emitVerilog(new RhlsBuf(1, 128, false))
//  emitVerilog(new RhlsBuf(1, 128, true))
//  emitVerilog(new RhlsBuf(2, 1, false))
//  emitVerilog(new RhlsBuf(2, 1, true))
//  emitVerilog(new RhlsBuf(2, 2, false))
//  emitVerilog(new RhlsBuf(2, 2, true))
//  emitVerilog(new RhlsBuf(2, 8, false))
//  emitVerilog(new RhlsBuf(2, 8, true))
//  emitVerilog(new RhlsBuf(2, 16, false))
//  emitVerilog(new RhlsBuf(2, 16, true))
//  emitVerilog(new RhlsBuf(2, 32, false))
//  emitVerilog(new RhlsBuf(2, 32, true))
//  emitVerilog(new RhlsBuf(2, 64, false))
//  emitVerilog(new RhlsBuf(2, 64, true))
//  emitVerilog(new RhlsBuf(2, 128, false))
//  emitVerilog(new RhlsBuf(2, 128, true))
//  emitVerilog(new RhlsBuf(4, 1, false))
//  emitVerilog(new RhlsBuf(4, 1, true))
//  emitVerilog(new RhlsBuf(4, 2, false))
//  emitVerilog(new RhlsBuf(4, 2, true))
//  emitVerilog(new RhlsBuf(4, 8, false))
//  emitVerilog(new RhlsBuf(4, 8, true))
//  emitVerilog(new RhlsBuf(4, 16, false))
//  emitVerilog(new RhlsBuf(4, 16, true))
//  emitVerilog(new RhlsBuf(4, 32, false))
//  emitVerilog(new RhlsBuf(4, 32, true))
//  emitVerilog(new RhlsBuf(4, 64, false))
//  emitVerilog(new RhlsBuf(4, 64, true))
//  emitVerilog(new RhlsBuf(4, 128, false))
//  emitVerilog(new RhlsBuf(4, 128, true))
//  emitVerilog(new RhlsBuf(8, 1, false))
//  emitVerilog(new RhlsBuf(8, 1, true))
//  emitVerilog(new RhlsBuf(8, 2, false))
//  emitVerilog(new RhlsBuf(8, 2, true))
//  emitVerilog(new RhlsBuf(8, 8, false))
//  emitVerilog(new RhlsBuf(8, 8, true))
//  emitVerilog(new RhlsBuf(8, 16, false))
//  emitVerilog(new RhlsBuf(8, 16, true))
//  emitVerilog(new RhlsBuf(8, 32, false))
//  emitVerilog(new RhlsBuf(8, 32, true))
//  emitVerilog(new RhlsBuf(8, 64, false))
//  emitVerilog(new RhlsBuf(8, 64, true))
//  emitVerilog(new RhlsBuf(8, 128, false))
//  emitVerilog(new RhlsBuf(8, 128, true))
//  emitVerilog(new RhlsBuf(16, 1, false))
//  emitVerilog(new RhlsBuf(32, 1, false))
//  emitVerilog(new RhlsBuf(64, 1, false))
//  emitVerilog(new RhlsBuf(16, 1, true))
//  emitVerilog(new RhlsBuf(32, 1, true))
//  emitVerilog(new RhlsBuf(64, 1, true))
//  emitVerilog(new RhlsBuf(16, 2, false))
//  emitVerilog(new RhlsBuf(32, 2, false))
//  emitVerilog(new RhlsBuf(64, 2, false))
//  emitVerilog(new RhlsBuf(16, 2, true))
//  emitVerilog(new RhlsBuf(32, 2, true))
//  emitVerilog(new RhlsBuf(64, 2, true))
//  emitVerilog(new RhlsBuf(16, 8, false))
//  emitVerilog(new RhlsBuf(32, 8, false))
//  emitVerilog(new RhlsBuf(64, 8, false))
//  emitVerilog(new RhlsBuf(16, 8, true))
//  emitVerilog(new RhlsBuf(32, 8, true))
//  emitVerilog(new RhlsBuf(64, 8, true))
//  emitVerilog(new RhlsBuf(16, 16, false))
//  emitVerilog(new RhlsBuf(32, 16, false))
//  emitVerilog(new RhlsBuf(64, 16, false))
//  emitVerilog(new RhlsBuf(16, 16, true))
//  emitVerilog(new RhlsBuf(32, 16, true))
//  emitVerilog(new RhlsBuf(64, 16, true))
//  emitVerilog(new RhlsBuf(16, 32, false))
//  emitVerilog(new RhlsBuf(32, 32, false))
//  emitVerilog(new RhlsBuf(64, 32, false))
//  emitVerilog(new RhlsBuf(16, 32, true))
//  emitVerilog(new RhlsBuf(32, 32, true))
//  emitVerilog(new RhlsBuf(64, 32, true))
//  emitVerilog(new RhlsBuf(16, 64, false))
//  emitVerilog(new RhlsBuf(32, 64, false))
//  emitVerilog(new RhlsBuf(64, 64, false))
//  emitVerilog(new RhlsBuf(16, 64, true))
//  emitVerilog(new RhlsBuf(32, 64, true))
//  emitVerilog(new RhlsBuf(64, 64, true))
//  emitVerilog(new RhlsBuf(16, 128, false))
//  emitVerilog(new RhlsBuf(32, 128, false))
//  emitVerilog(new RhlsBuf(64, 128, false))
//  emitVerilog(new RhlsBuf(16, 128, true))
//  emitVerilog(new RhlsBuf(32, 128, true))
//  emitVerilog(new RhlsBuf(64, 128, true))
//  emitVerilog(new RhlsBuf(128, 1, false))
//  emitVerilog(new RhlsBuf(128, 1, true))
//  emitVerilog(new RhlsBuf(128, 2, false))
//  emitVerilog(new RhlsBuf(128, 2, true))
//  emitVerilog(new RhlsBuf(128, 8, false))
//  emitVerilog(new RhlsBuf(128, 8, true))
//  emitVerilog(new RhlsBuf(128, 16, false))
//  emitVerilog(new RhlsBuf(128, 16, true))
//  emitVerilog(new RhlsBuf(128, 32, false))
//  emitVerilog(new RhlsBuf(128, 32, true))
//  emitVerilog(new RhlsBuf(128, 64, false))
//  emitVerilog(new RhlsBuf(128, 64, true))
//  emitVerilog(new RhlsBuf(128, 128, false))
//  emitVerilog(new RhlsBuf(128, 128, true))
//  emitVerilog(new RhlsBuf(256, 1, false))
//  emitVerilog(new RhlsBuf(256, 1, true))
//  emitVerilog(new RhlsBuf(256, 2, false))
//  emitVerilog(new RhlsBuf(256, 2, true))
//  emitVerilog(new RhlsBuf(256, 8, false))
//  emitVerilog(new RhlsBuf(256, 8, true))
//  emitVerilog(new RhlsBuf(256, 16, false))
//  emitVerilog(new RhlsBuf(256, 16, true))
//  emitVerilog(new RhlsBuf(256, 32, false))
//  emitVerilog(new RhlsBuf(256, 32, true))
//  emitVerilog(new RhlsBuf(256, 64, false))
//  emitVerilog(new RhlsBuf(256, 64, true))
//  emitVerilog(new RhlsBuf(256, 128, false))
//  emitVerilog(new RhlsBuf(256, 128, true))
//  emitVerilog(new RhlsBuf(512, 1, false))
//  emitVerilog(new RhlsBuf(512, 1, true))
//  emitVerilog(new RhlsBuf(512, 2, false))
//  emitVerilog(new RhlsBuf(512, 2, true))
//  emitVerilog(new RhlsBuf(512, 8, false))
//  emitVerilog(new RhlsBuf(512, 8, true))
//  emitVerilog(new RhlsBuf(512, 16, false))
//  emitVerilog(new RhlsBuf(512, 16, true))
//  emitVerilog(new RhlsBuf(512, 32, false))
//  emitVerilog(new RhlsBuf(512, 32, true))
//  emitVerilog(new RhlsBuf(512, 64, false))
//  emitVerilog(new RhlsBuf(512, 64, true))
//  emitVerilog(new RhlsBuf(512, 128, false))
//  emitVerilog(new RhlsBuf(512, 128, true))
//  emitVerilog(new RhlsBuf(1024, 1, false))
//  emitVerilog(new RhlsBuf(1024, 1, true))
//  emitVerilog(new RhlsBuf(1024, 2, false))
//  emitVerilog(new RhlsBuf(1024, 2, true))
//  emitVerilog(new RhlsBuf(1024, 8, false))
//  emitVerilog(new RhlsBuf(1024, 8, true))
//  emitVerilog(new RhlsBuf(1024, 16, false))
//  emitVerilog(new RhlsBuf(1024, 16, true))
//  emitVerilog(new RhlsBuf(1024, 32, false))
//  emitVerilog(new RhlsBuf(1024, 32, true))
//  emitVerilog(new RhlsBuf(1024, 64, false))
//  emitVerilog(new RhlsBuf(1024, 64, true))
//  emitVerilog(new RhlsBuf(1024, 128, false))
//  emitVerilog(new RhlsBuf(1024, 128, true))
//  emitVerilog(new RhlsBuf(2048, 1, false))
//  emitVerilog(new RhlsBuf(2048, 1, true))
//  emitVerilog(new RhlsBuf(2048, 2, false))
//  emitVerilog(new RhlsBuf(2048, 2, true))
//  emitVerilog(new RhlsBuf(2048, 8, false))
//  emitVerilog(new RhlsBuf(2048, 8, true))
//  emitVerilog(new RhlsBuf(2048, 16, false))
//  emitVerilog(new RhlsBuf(2048, 16, true))
//  emitVerilog(new RhlsBuf(2048, 32, false))
//  emitVerilog(new RhlsBuf(2048, 32, true))
//  emitVerilog(new RhlsBuf(2048, 64, false))
//  emitVerilog(new RhlsBuf(2048, 64, true))
//  emitVerilog(new RhlsBuf(2048, 128, false))
//  emitVerilog(new RhlsBuf(2048, 128, true))

//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_256_float_I64W_I32W_O32W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_128_float_I64W_I32W_O32W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_64_float_I64W_I32W_O32W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_32_float_I64W_I32W_O32W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_16_float_I64W_I32W_O32W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_8_float_I64W_I32W_O32W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_4_float_I64W_I32W_O32W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_2_float_I64W_I32W_O32W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_1_float_I64W_I32W_O32W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_256_bit64_I64W_I64W_O64W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_128_bit64_I64W_I64W_O64W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_64_bit64_I64W_I64W_O64W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_32_bit64_I64W_I64W_O64W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_16_bit64_I64W_I64W_O64W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_8_bit64_I64W_I64W_O64W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_4_bit64_I64W_I64W_O64W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_2_bit64_I64W_I64W_O64W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_1_bit64_I64W_I64W_O64W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_256_bit32_I64W_I32W_O32W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_128_bit32_I64W_I32W_O32W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_64_bit32_I64W_I32W_O32W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_32_bit32_I64W_I32W_O32W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_16_bit32_I64W_I32W_O32W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_8_bit32_I64W_I32W_O32W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_4_bit32_I64W_I32W_O32W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_2_bit32_I64W_I32W_O32W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_1_bit32_I64W_I32W_O32W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_256_bit16_I64W_I16W_O16W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_128_bit16_I64W_I16W_O16W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_64_bit16_I64W_I16W_O16W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_32_bit16_I64W_I16W_O16W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_16_bit16_I64W_I16W_O16W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_8_bit16_I64W_I16W_O16W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_4_bit16_I64W_I16W_O16W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_2_bit16_I64W_I16W_O16W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_1_bit16_I64W_I16W_O16W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_256_bit8_I64W_I8W_O8W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_128_bit8_I64W_I8W_O8W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_64_bit8_I64W_I8W_O8W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_32_bit8_I64W_I8W_O8W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_16_bit8_I64W_I8W_O8W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_8_bit8_I64W_I8W_O8W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_4_bit8_I64W_I8W_O8W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_2_bit8_I64W_I8W_O8W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_1_bit8_I64W_I8W_O8W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_256_fixedvector_bit32_4__I64W_I128W_O128W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_128_fixedvector_bit32_4__I64W_I128W_O128W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_64_fixedvector_bit32_4__I64W_I128W_O128W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_32_fixedvector_bit32_4__I64W_I128W_O128W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_16_fixedvector_bit32_4__I64W_I128W_O128W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_8_fixedvector_bit32_4__I64W_I128W_O128W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_4_fixedvector_bit32_4__I64W_I128W_O128W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_2_fixedvector_bit32_4__I64W_I128W_O128W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_1_fixedvector_bit32_4__I64W_I128W_O128W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_256_ptr_I64W_I64W_O64W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_128_ptr_I64W_I64W_O64W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_64_ptr_I64W_I64W_O64W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_32_ptr_I64W_I64W_O64W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_16_ptr_I64W_I64W_O64W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_8_ptr_I64W_I64W_O64W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_4_ptr_I64W_I64W_O64W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_2_ptr_I64W_I64W_O64W_O64W"))
//  emitVerilog(RhlsDecLoad.from_name("op_HLS_DEC_LOAD_1_ptr_I64W_I64W_O64W_O64W"))
//  emitVerilog(new RhlsBuf(1, 1, false))
//  emitVerilog(new RhlsBuf(1, 1, true))
//  emitVerilog(new RhlsBuf(1, 32, false))
//  emitVerilog(new RhlsBuf(1, 32, true))
//  emitVerilog(new RhlsBuf(128, 1, false))
//  emitVerilog(new RhlsBuf(128, 1, true))
//  emitVerilog(new RhlsBuf(64, 32, true))
//  emitVerilog(new RhlsBuf(0, 1, true))
//  emitVerilog(new RhlsBuf(0, 32, true))
//  emitVerilog(new op_SIToFP_I32W_O32W)
}
