import complex._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import fourier.Chirp
import scala.util.Random
import spire.math.Complex
import spire.implicits._

class Calibrate(len : Int) extends Component {
  val px = in(Vec.fill(len)(SFix(1 exp, -16 exp)))
  val py = in(Vec.fill(len)(SFix(1 exp, -16 exp)))
  val theta_hat = out(Cx())
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
  theta_hat.re := (slicesum.s.re * invSliceLen).truncated
  theta_hat.im := (slicesum.s.im * invSliceLen).truncated
}

object Demo extends App {
  val n = 3 + Random.nextInt(10)
  val px = Seq.fill(n)(Random.nextDouble())
  val py = Seq.fill(n)(Random.nextDouble())
  val ref = new reference.Calibrate(px, py)
  println(s"px = $px")
  println(s"py = $py")
  println(s"reference theta-hat = ${ref.theta_hat}")
  println(s"reference phi-hat = ${ref.phi_hat}")
  SimConfig.compile { new Calibrate(n) }.doSim { dut =>
    for(i <- 0 until n) {
      dut.px(i) #= px(i)
      dut.py(i) #= py(i)
    }
    sleep(1)
    val theta_hat = Complex(dut.theta_hat.re.toDouble,
      dut.theta_hat.im.toDouble)
    println(f"computed theta_hat = $theta_hat")
  }
}
