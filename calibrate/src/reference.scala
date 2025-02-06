package reference
import scala.util.Random
import scala.math.Pi
import scala.math.min
import spire.math.Complex
import spire.implicits._

class FourierTransform(val xs : Seq[Complex[Double]]) {
  val n = xs.length

  def ith_freq(i : Int) : Complex[Double] = {
    var s = Complex.zero[Double]
    for(j <- 0 until n) {
      val phase = Complex.polar(1.0, -2 * Pi * i * j / n)
      s = s + xs(j) * phase
    }
    return s
  }

  def compute() : Seq[Complex[Double]] = Seq.tabulate(n)(ith_freq)
}

class Triagonal(val n: Int) {
  def get_inverse(i: Int, j: Int) : Double = {
    // 1-indexed pen-and-paper formula...
    val i1 = i + 1
    val j1 = j + 1
    return min(i1, j1) - i1.toDouble * j1 / (n + 1)
  }

  def sum_inverse_entries() : Double = {
    return (n * (n + 1) * (n + 2)).toDouble / 12
  }

  def inverse_action_ith(i: Int, v: Seq[Complex[Double]]) : Complex[Double] = {
    var s = Complex.zero[Double]
    for(j <- 0 until n) {
      s = s + get_inverse(i, j) * v(j)
    }
    return s
  }

  def inverse_action(v: Seq[Complex[Double]]) : Seq[Complex[Double]] = {
    return Seq.tabulate(n)(i => inverse_action_ith(i, v))
  }
}

class Calibrate (px : Seq[Double], py : Seq[Double]) {
  val n = px.length
  val d = n / 2
  val hs = Seq.tabulate(n)(i => Complex(px(i) - 0.5, py(i) - 0.5))
  val fft = new FourierTransform(hs)
  val c = fft.compute()
  val slice = c.slice(0, d)
  val theta_hat = slice.reduce(_ + _) / slice.length
  val deltas = Seq.tabulate(d - 1)(i => (c(i) * c(i + 1).conjugate).arg)
  val trigonal = new Triagonal(d - 1)
  val numerator = trigonal.inverse_action(slice).reduce(_ + _)
  val denominator = 2 * trigonal.sum_inverse_entries()
  val phi_hat = numerator / denominator
}

object Test extends App {
  val n = 5
  val px = Seq.fill(n)(Random.nextDouble())
  val py = Seq.fill(n)(Random.nextDouble())
  val calibrate = new Calibrate(px, py)
  println(s"px = $px")
  println(s"py = $py")
  println(s"theta-hat = ${calibrate.theta_hat}")
  println(s"phi_hat = ${calibrate.phi_hat}")
}
