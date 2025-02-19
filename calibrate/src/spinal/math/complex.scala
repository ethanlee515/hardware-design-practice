package spinalmath

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spire.math.Complex
import spire.implicits._

case class Cx() extends Bundle {
  val re = SFix(16 exp, -16 exp)
  val im = SFix(16 exp, -16 exp)

  def setval(z: Complex[Double]) {
    re := z.real
    im := z.imag
  }
}

class CxAdd extends Component {
  val z1 = in(Cx())
  val z2 = in(Cx())
  val s = out(Cx())
  s.re := (z1.re + z2.re).truncated
  s.im := (z1.im + z2.im).truncated
}

class CxSub extends Component {
  val z1 = in(Cx())
  val z2 = in(Cx())
  val d = out(Cx())
  d.re := (z1.re - z2.re).truncated
  d.im := (z1.im - z2.im).truncated
}

class CxMul extends Component {
  val z1 = in(Cx())
  val z2 = in(Cx())
  val prod = out(Cx())
  prod.re := (z1.re * z2.re - z1.im * z2.im).truncated
  prod.im := (z1.re * z2.im + z1.im * z2.re).truncated
}

class CxDiv2 extends Component {
  val z = in(Cx())
  val z2 = out(Cx())
  z2.re := (z.re >> 1).truncated
  z2.im := (z.im >> 1).truncated
}

class CxVecAdd(n: Int) extends Component {
  val zs = in(Vec.fill(n)(Cx()))
  val s = out(Cx())
  
  var terms = zs
  while(terms.size != 1) {
    val new_sz = (terms.size + 1) / 2
    val next_terms = Vec.fill(new_sz)(Cx())
    for(j <- 0 until new_sz) {
      if(2 * j + 1 != terms.size) {
        next_terms(j).re := terms(2 * j).re + terms(2 * j + 1).re
        next_terms(j).im := terms(2 * j).im + terms(2 * j + 1).im
      } else {
        next_terms(j) := terms(2 * j)
      }
    }
    terms = next_terms
  }
  s := terms(0)
}

object TestComplex extends App {
  val n = scala.util.Random.nextInt(8) + 3
  def randc() : Complex[Double] = {
    val re = -2 + 4 * scala.util.Random.nextDouble()
    val im = -2 + 4 * scala.util.Random.nextDouble()
    return Complex(re, im)
  }
  val arr = Seq.fill(n)(randc())
  val s = arr.reduce(_ + _)

  println(f"arr = $arr")
  println(f"sum = $s")

  SimConfig.compile{ new CxVecAdd(n) }.doSim { dut =>
    for(i <- 0 until n) {
      dut.zs(i).re #= arr(i).real
      dut.zs(i).im #= arr(i).imag
    }
    sleep(1)
    println(dut.s.re.toDouble)
    println(dut.s.im.toDouble)
  }
}
