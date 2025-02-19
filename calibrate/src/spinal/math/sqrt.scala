package spinalmath

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spire.math.Complex
import spire.implicits._

class Sqrt extends Component {
  val x = in(SFix(16 exp, -16 exp))
  val y = out(SFix(16 exp, -16 exp))
  var a = SFix(16 exp, -16 exp)
  val half = SFix(16 exp, -16 exp)
  half := 0.5
  val half_x = (half * x).truncated
  a := 1.0
  for(_ <- 0 until 10) {
    var next = SFix(16 exp, -16 exp)
    val three_halves = SFix(16 exp, -16 exp)
    three_halves := 1.5
    next := (a * (three_halves - half_x * a * a)).truncated
    a = next
  }
  val neg_a = SFix(16 exp, -16 exp)
  val z = SFix(16 exp, -16 exp)
  z := 0
  neg_a := z - a
  val abs_a = SFix(16 exp, -16 exp)
  abs_a := (a < 0) ? neg_a | a
  y := (abs_a * x).truncated
}

object TestSqrt extends App {
  val x = 0.1 + 4 * scala.util.Random.nextDouble()
  println(s"sqrt(${x}) = ${scala.math.sqrt(x)}")
  SimConfig.compile { new Sqrt() }.doSim { dut =>
    dut.x #= x
    sleep(1)
    println(s"computed = ${dut.y.toDouble}")
  }
}
