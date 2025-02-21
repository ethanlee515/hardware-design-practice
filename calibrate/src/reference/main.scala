package reference
import fourier.FourierTransform
import scala.util.Random
import scala.math.min
import spire.math.Complex
import spire.implicits._

class Tridiagonal(val n: Int) {
  def get_inverse(i: Int, j: Int) : Double = {
    // 1-indexed pen-and-paper formula...
    val i1 = i + 1
    val j1 = j + 1
    return min(i1, j1) - i1.toDouble * j1 / (n + 1)
  }

  def sum_inverse_entries() : Double = {
    return (n * (n + 1) * (n + 2)).toDouble / 12
  }

  def sum_inverse_column(col : Int) : Double = {
    var s = 0.0
    for(i <- 0 until n) {
      s += get_inverse(i, col)
    }
    return s
  }
}

class Calibrate (px : Seq[Double], py : Seq[Double]) {
  val n = px.length
  val d = n / 2
  val hs = Seq.tabulate(n)(i => Complex(px(i) - 0.5, py(i) - 0.5))
  val fft = new FourierTransform(hs)
  val c = fft.compute()
  val slice = c.slice(0, d)
  val slice_mod = slice.map(_.abs)
  val theta_hat = slice_mod.reduce(_ + _) / slice.length
  val deltas = Seq.tabulate(d - 1)(i => (c(i) * c(i + 1).conjugate).arg)
  val tridiagonal = new Tridiagonal(d - 1)
  val denominator = 2 * tridiagonal.sum_inverse_entries()
  var numerator = 0.0
  for(i <- 0 until (d - 1)) {
    numerator = numerator + deltas(i) * tridiagonal.sum_inverse_column(i)
  }
  val phi_hat = numerator / denominator
}

object Test extends App {
  val n = 13
  val px = Seq.fill(n)(Random.nextDouble())
  val py = Seq.fill(n)(Random.nextDouble())
  println(s"px = $px")
  println(s"py = $py")
  val calibrate = new Calibrate(px, py)
  println(s"theta-hat = ${calibrate.theta_hat}")
  println(s"phi-hat = ${calibrate.phi_hat}")
}
