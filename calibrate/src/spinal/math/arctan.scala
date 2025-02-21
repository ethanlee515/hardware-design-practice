package spinalmath

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import scala.math.atan2
import scala.math.Pi
import scala.util.Random
import spire.math.Complex
import spire.implicits._
import reference.ATanPrecompute

class ATan2 extends Component {
  val y = in(SFix(16 exp, -16 exp))
  val x = in(SFix(16 exp, -16 exp))
  val result = out(SFix(16 exp, -16 exp))
  var acc = SFix(16 exp, -16 exp)
  acc := 0
  val q2 = (x < 0 && y > 0)
  val q3 = (x < 0 && y < 0)
  var zero = SFix(16 exp, -16 exp)
  zero := 0.0
  var xv = SFix(16 exp, -16 exp)
  xv := (q2 || q3) ? (zero - x).truncated | x
  var yv = SFix(16 exp, -16 exp)
  yv := (q2 || q3) ? (zero - y).truncated | y
  for(i <- 0 until 20) {
    val d = (yv > 0)
    val x_new = SFix(16 exp, -16 exp)
    val y_new = SFix(16 exp, -16 exp)
    val acc_new = SFix(16 exp, -16 exp)
    val f = SFix(16 exp, -16 exp)
    f := ATanPrecompute.get(i)
    val rs = SFix(16 exp, -16 exp)
    rs := (2.0 ** (-i))
    when(d) {
      x_new := (xv + yv * rs).truncated
      y_new := (yv - xv * rs).truncated
      acc_new := (acc + f).truncated
    } otherwise {
      x_new := (xv - yv * rs).truncated
      y_new := (yv + xv * rs).truncated
      acc_new := (acc - f).truncated
    }
    xv = x_new
    yv = y_new
    acc = acc_new
  }
  var pi = SFix(16 exp, -16 exp)
  pi := Pi
  when(q2) {
    result := (acc + pi).truncated
  } elsewhen(q3) {
    result := (acc - pi).truncated
  } otherwise {
    result := acc
  }
}

object TestATan2 extends App {
  val x = -2.0 + 4.0 * Random.nextDouble()
  val y = -2.0 + 4.0 * Random.nextDouble()
  println(s"reference atan2($y, $x) = ${atan2(y, x)}")
  SimConfig.compile { new ATan2() }.doSim { dut =>
    dut.y #= y
    dut.x #= x
    sleep(1)
    println(s"computed = ${dut.result.toDouble}")
  }
}
