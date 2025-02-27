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

