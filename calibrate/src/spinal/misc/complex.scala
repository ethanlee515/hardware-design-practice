package complex

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spire.math.Complex

case class Cx() extends Bundle {
  val re = SFix(8 exp, -24 exp)
  val im = SFix(8 exp, -24 exp)

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

object TestComplex extends App {
  SimConfig.compile{ new CxMul }.doSim { dut =>
    dut.z1.re #= 0.1
    dut.z1.im #= -0.2
    dut.z2.re #= 0.3
    dut.z2.im #= 0.4
    sleep(1)
    println(dut.prod.re.toDouble)
    println(dut.prod.im.toDouble)
  }
}
