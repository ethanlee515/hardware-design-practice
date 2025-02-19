import spinal.core._
import spinal.core.sim._
import spinal.lib._
import fourier.Chirp
import scala.util.Random
import spire.math.Complex
import spire.implicits._
import spinalmath._

class Calibrate(len : Int) extends Component {
  val px = in(Vec.fill(len)(SFix(1 exp, -16 exp)))
  val py = in(Vec.fill(len)(SFix(1 exp, -16 exp)))
  val theta_hat = out(SFix(16 exp, -16 exp))
  val d = len / 2
  val hs = Vec.fill(len)(Cx())
  for(i <- 0 until len) {
    val half = SFix(1 exp, -2 exp)
    half := 0.5
    hs(i).re := (px(i) - half).truncated
    hs(i).im := (py(i) - half).truncated
  }
  val fft = new Chirp(len)
  fft.xs := hs
  val slice = Vec.fill(d)(Cx())
  for(i <- 0 until d) {
    slice(i) := fft.ys(i)
  }
  val slicesum = new CxVecAdd(d)
  slicesum.zs := slice
  val invSliceLen = SFix(1 exp, -16 exp)
  invSliceLen := 1.0 / d
  // TODO wrong
  theta_hat := (slicesum.s.re * invSliceLen).truncated
//  theta_hat.im := (slicesum.s.im * invSliceLen).truncated
  val deltas = Vec.fill(d - 1)(Cx())
  /*
  for(i <- 0 until d - 1) {
    val mul = new CxMul()
    mul.z1 := slice(i)
    mul.z2.re := slice(i + 1).re
    mul.z2.im := - slice(i + 1).im
    // TODO arg...
  }
  */
}

object Demo extends App {
  val n = 5 + Random.nextInt(10)
  val px = Seq.fill(n)(Random.nextDouble())
  val py = Seq.fill(n)(Random.nextDouble())
  println(s"px = $px")
  println(s"py = $py")
  val ref = new reference.Calibrate(px, py)
  println(s"reference theta-hat = ${ref.theta_hat}")
  println(s"reference phi-hat = ${ref.phi_hat}")
  SimConfig.compile { new Calibrate(n) }.doSim { dut =>
    for(i <- 0 until n) {
      dut.px(i) #= px(i)
      dut.py(i) #= py(i)
    }
    sleep(1)
    val theta_hat = dut.theta_hat.toDouble
    println(f"computed theta_hat = $theta_hat")
  }
}
