package floatingpoint

import scala.math.pow
import scala.math.log
import scala.math.round

import chisel3._
import chisel3.util._

object FloatingPoint {

    val nan :: an :: ninfinity :: infinity :: zero :: Nil = Enum(5)

    private def log2(x: Int): Int = (round(log(x)/log(2))).toInt

    private def sum(vector: Array[UInt]): UInt = {
        val length = log2(vector.length+1)
        val temp = Array.tabulate(vector.length)(n => UInt(length.W))
        temp(0) = Cat(0.U ((length-1).W), vector(0))
        for (i <- 1 to vector.length-1) {
            temp(i) = Cat(0.U ((length-1).W), vector(i))+temp(i-1)
        }
        return temp(vector.length-1)
    }

    private def countZerosFromTheLeft(value: UInt): UInt = {
        val sequence = VecInit((~value).asBools)
        val res = Array.tabulate(sequence.getWidth)(n => Wire(UInt(1.W)))
        for (i <- 0 to sequence.getWidth-1) {
            if(i == sequence.getWidth-1) {
                res(i) := sequence(i)
            }
            else{
                res(i) := res(i+1)&sequence(i)
            }
        }
        return sum(res)+1.U // '+1.U' due to the implicit '1'
    }

    implicit class UIntToFloatingPoint(elem: UInt) {

        def toFloatingPoint(exp: Int, man: Int): FloatingPoint = {
            val res = Wire(new FloatingPoint(exp, man))
            when(elem === 0.U) {
                res := (0.U).asTypeOf(new FloatingPoint(exp, man))
            }
            .otherwise {
                val shifts = countZerosFromTheLeft(elem(man-1, 0))
                res.mantissa := elem << shifts
                res.exponent := (((pow(2, exp-1)-1).toInt)+man).U-shifts
                res.sign := 0.U
            }
            return res
        }
    }

    implicit class SIntToFloatingPoint(elem: SInt) {

        def toFloatingPoint(exp: Int, man: Int): FloatingPoint = {
            val sign = (elem < 0.S)
            val representation = Mux(sign, ~(elem.asUInt-1.U), elem.asUInt)
            val res = representation.toFloatingPoint(exp, man)
            res.sign := sign
            return res
        }
    }

}

class FloatingPoint(exp: Int, man: Int) extends Bundle {

    val sign = Bool()
    val exponent = UInt(exp.W)
    val mantissa = UInt(man.W)

    def identify(f: FloatingPoint): UInt = {
        val res = Wire(UInt(3.W))
        when(f.exponent === ("b"+("1"*exp)).U) {
            when(f.mantissa === 0.U) {
                when(f.sign === 0.U) {
                    res := FloatingPoint.infinity
                }
                .otherwise{
                    res := FloatingPoint.ninfinity
                }
            }
            .otherwise{
                res := FloatingPoint.nan
            }
        }.elsewhen(f.exponent === 0.U && f.mantissa === 0.U){
            res := FloatingPoint.zero
          }
        .otherwise {
            res := FloatingPoint.an
        }
        return res
    }

    def absoluteValue(a: SInt): UInt = {
        val res = Wire(UInt())
        when(a < 0.S) {
            res := (-a).asUInt
        }
        .otherwise {
            res := a.asUInt
        }
        return res
    }

    def toInt(a: UInt, s: UInt): SInt = {
        val res = Wire(SInt())
        when(s(0).asBool) {
            res := -(a.asSInt)
        }
        .otherwise{
            res := a.asSInt
        }
        return res
    }

//    override def cloneType = (new FloatingPoint(this.exp, this.man)).asInstanceOf[this.type]

    def +(that: FloatingPoint): FloatingPoint = {
        val res = Wire(new FloatingPoint(this.exp, this.man))
        // SETUP
        val Aidentity = Wire(UInt(3.W))
        val Bidentity = Wire(UInt(3.W))
        Aidentity := identify(this)
        Bidentity := identify(that)
        when((Aidentity === FloatingPoint.infinity & Bidentity === FloatingPoint.ninfinity)|(Bidentity === FloatingPoint.infinity & Aidentity === FloatingPoint.ninfinity)) {
            res := (("b"+("1"*(1+exp+1))+("0"*(man-1))).U).asTypeOf(new FloatingPoint(this.exp, this.man))
        }
        .elsewhen(Aidentity === FloatingPoint.infinity | Aidentity === FloatingPoint.ninfinity) {
            res := this
        }
        .elsewhen(Bidentity === FloatingPoint.infinity | Bidentity === FloatingPoint.ninfinity) {
            res := that
        }
        .elsewhen(Aidentity === FloatingPoint.zero) {
            res := that
        }
        .elsewhen(Bidentity === FloatingPoint.zero) {
            res := this
        }
        .otherwise {
            // INIT
            val exponent   = Wire(UInt(this.exp.W))
            val difference = Wire(UInt(this.exp.W))
            val a_mantissa = Wire(UInt((this.man+3).W))
            val b_mantissa = Wire(UInt((this.man+3).W))
            val mantissa   = Wire(UInt((this.man+3).W))
            val sum        = Wire(SInt((this.man+3).W))
            val rev_man    = Wire(UInt((this.man+3).W))
            difference := absoluteValue((this.exponent).asSInt-(that.exponent).asSInt)
            // SHIFT EXPONENT
            when(this.exponent > that.exponent) {
                exponent := this.exponent
                a_mantissa := Cat(1.U(1.W), this.mantissa)
                b_mantissa := Cat(1.U(1.W), that.mantissa) >> difference
            }
            .otherwise{
                exponent := that.exponent;
                a_mantissa := Cat(1.U(1.W), this.mantissa) >> difference
                b_mantissa := Cat(1.U(1.W), that.mantissa)
            }
            dontTouch(a_mantissa).suggestName("a_mantissa")
            dontTouch(b_mantissa).suggestName("b_mantissa")
            val a_mantissa_signed = WireInit(toInt(a_mantissa, this.sign))
            val b_mantissa_signed = WireInit(toInt(b_mantissa, that.sign))
            dontTouch(a_mantissa_signed).suggestName("a_mantissa_signed")
            dontTouch(b_mantissa_signed).suggestName("b_mantissa_signed")
            dontTouch(sum).suggestName("sum")
            sum := a_mantissa_signed+b_mantissa_signed
            mantissa := absoluteValue(sum)
            rev_man  := Reverse(mantissa)
            when(mantissa(man+1)) {
                res.exponent := Mux(sum =/= 0.S, exponent, 0.U)+1.U
                res.mantissa := ((mantissa)+1.U)>>1.U
            } .otherwise {
              when(mantissa(man))
              {
                  res.exponent := Mux(sum =/= 0.S, exponent, 0.U)
                  res.mantissa := mantissa(man-1,0)
              }
                .otherwise
                {
                    res.exponent := Mux(sum =/= 0.S, exponent, 0.U) - PriorityEncoder(rev_man)+2.U
                    res.mantissa := (mantissa << (PriorityEncoder(rev_man)-2.U))(man-1,0)
                }

            }
            res.sign := (sum < 0.S)
//            // MANTISSA COMPUTATION
//            res.mantissa := Mux(mantissa(man+1), ((mantissa)+1.U)>>1.U, mantissa)(man-1, 0)
//            // SIGN MANAGMENT
//            res.sign := (sum < 0.S)
//            // EXPONENT MANAGMENT
//            res.exponent := Mux(sum =/= 0.S, exponent, 0.U)+Cat(0.U((exp-1).W), mantissa(man+1))
        }
        return res
    }

    def neg(): FloatingPoint = {
        val tmp = WireInit(this)
        tmp.sign := ~this.sign
        return tmp
    }

    def -(that: FloatingPoint): FloatingPoint = {
        return this + that.neg()
    }

    def *(that: FloatingPoint): FloatingPoint = {
        // SETUP
        val res = Wire(new FloatingPoint(this.exp, this.man))
        val Aidentity = Wire(UInt(3.W))
        val Bidentity = Wire(UInt(3.W))
        Aidentity := identify(this)
        Bidentity := identify(that)
        when(Aidentity === FloatingPoint.infinity | Aidentity === FloatingPoint.ninfinity | Bidentity === FloatingPoint.infinity | Bidentity === FloatingPoint.ninfinity) {
            res := (Cat((this.sign^that.sign), ("b0"+("1"*(exp))+("0"*(man))).U((exp+man).W))).asTypeOf(new FloatingPoint(this.exp, this.man))
        }
        .elsewhen(Aidentity === FloatingPoint.nan) {
            res := (Cat(this.sign, ("b"+("1"*(exp+1))+("0"*(man-1))).U((exp+man).W))).asTypeOf(new FloatingPoint(this.exp, this.man))
        }
        .elsewhen(Bidentity === FloatingPoint.nan) {
            res := (Cat(that.sign, ("b"+("1"*(exp+1))+("0"*(man-1))).U((exp+man).W))).asTypeOf(new FloatingPoint(this.exp, this.man))
        }
        .elsewhen(Aidentity === FloatingPoint.zero || Bidentity === FloatingPoint.zero) {
            res := (0.U).asTypeOf(new FloatingPoint(this.exp, this.man))
        }
        .otherwise {
            res.sign := (this.sign^that.sign)
            val mantissa = (Cat(1.U(1.W), this.mantissa)*Cat(1.U(1.W), that.mantissa)) >> man.U
            res.mantissa := Mux(mantissa(man+1).asBool, (mantissa+1.U)>>1.U, mantissa)(man-1, 0)
            val exponent = this.exponent+that.exponent
            res.exponent := exponent-((pow(2, exp-1)-1).toInt).U+mantissa(man+1)
        }
        return res
    }

    def toUInt(): UInt = {
        val difference = ((pow(2, this.exp-1)-1).toInt).U-this.exponent
        return Cat(0.U((1+this.exp).W), Cat(1.U(1.W), mantissa)>>((this.man).U+difference))
    }

    def toSInt(): SInt = {
        val res = this.toUInt()
        return Mux(this.sign, (~res)+1.U, res).asSInt
    }

}
