package reference.fourier
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

