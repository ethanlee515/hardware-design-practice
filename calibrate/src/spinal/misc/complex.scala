package misc

import spinal.core._
import spinal.core.sim._
import spinal.lib._

case class Cx() extends Bundle {
  val re = AFix.S(8 exp, -24 exp)
  val im = AFix.S(8 exp, -24 exp)
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
    dut.z1.im #= 0.1
    dut.z2.re #= 0.1
    dut.z2.im #= 0.1
    sleep(1)
    println(dut.prod.re.toDouble)
    println(dut.prod.im.toDouble)
  }
}
