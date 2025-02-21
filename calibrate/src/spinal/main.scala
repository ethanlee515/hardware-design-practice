import spinal.core._
import spinal.core.sim._
import spinal.lib._
import fourier.Chirp
import scala.util.Random
import spire.math.Complex
import spire.implicits._
import spinalmath._
import reference.Tridiagonal

class Calibrate(len : Int) extends Component {
  val px = in(Vec.fill(len)(SFix(1 exp, -16 exp)))
  val py = in(Vec.fill(len)(SFix(1 exp, -16 exp)))
  val theta_hat = out(SFix(16 exp, -16 exp))
  val phi_hat = out(SFix(16 exp, -16 exp))
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
  val slice_abs = Vec.fill(d)(SFix(16 exp, -16 exp))
  for(i <- 0 until d) {
    val sqrt = new Sqrt()
    sqrt.x := (slice(i).re * slice(i).re + slice(i).im * slice(i).im).truncated
    slice_abs(i) := sqrt.y
  }
  val slicesum = new VecAdd(d)
  slicesum.xs := slice_abs
  val invSliceLen = SFix(1 exp, -16 exp)
  invSliceLen := 1.0 / d
  theta_hat := (slicesum.s * invSliceLen).truncated
  val tridiagonal = new Tridiagonal(d - 1)
  val denominator = 2 * tridiagonal.sum_inverse_entries()
  val inv_denominator = SFix(16 exp, -16 exp)
  inv_denominator := 1.0 / denominator
  val deltas = Vec.fill(d - 1)(SFix(16 exp, -16 exp))
  for(i <- 0 until (d - 1)) {
    val mul = new CxMul()
    mul.z1 := slice(i)
    mul.z2.re := slice(i + 1).re
    val zero = SFix(16 exp, -16 exp)
    zero := 0.0
    mul.z2.im := (zero - slice(i + 1).im).truncated
    val atan2 = new ATan2()
    atan2.y := mul.prod.im
    atan2.x := mul.prod.re
    deltas(i) := atan2.result
  }
  val numerator_vec = Vec.fill(d - 1)(SFix(16 exp, -16 exp))
  for(i <- 0 until (d - 1)) {
    val f = SFix(16 exp, -16 exp)
    f := tridiagonal.sum_inverse_column(i)
    numerator_vec(i) := (deltas(i) * f).truncated
  }
  val vecAdd = new VecAdd(d - 1)
  vecAdd.xs := numerator_vec
  phi_hat := (vecAdd.s * inv_denominator).truncated
}
