package reference.fourier
import scala.math.Pi
import spire.math.Complex
import spire.implicits._

class ChirpPrecompute(xslen : Int) {
  val leastLen = 2 * xslen - 1
  val log2_leastLen = 31 - Integer.numberOfLeadingZeros(leastLen)
  val chirplen = 2 ** (log2_leastLen + 1)
  val primitive_root = Complex.polar(1.0, - 2 * Pi / xslen)
  def get_vn(n : Int) : Complex[Double] = {
    if(n <= xslen - 1) {
      return primitive_root ** (- n * n / 2.0)
    }
    if(n >= chirplen - xslen + 1) {
      return primitive_root ** (- ((chirplen - n) ** 2) / 2.0)
    }
    return Complex.zero[Double]
  }
  val vs = Seq.tabulate(chirplen)(get_vn)
  val ct = new CooleyTukey(vs)
  val vs_fft = ct.compute()
}

class Chirp(xs : Seq[Complex[Double]]) {
  val precompute = new ChirpPrecompute(xs.length)
  val chirplen = precompute.chirplen
  val w = precompute.primitive_root
  val ys1 = xs.zipWithIndex.map { case (x, i) =>
    x * (w ** (i * i / 2.0))
  }
  val ys = ys1 ++ Array.fill(chirplen - xs.length)(Complex.zero[Double])
  val ct = new CooleyTukey(ys)
  val ys_fft = ct.compute()
  val gs_fft = (ys_fft, precompute.vs_fft).zipped.map(_ * _)
  val ict = new InvCooleyTukey(gs_fft)
  val gs = ict.compute()
  val result = Seq.tabulate(xs.length) { k =>
    val w = precompute.primitive_root
    (w ** (k * k / 2.0)) * gs(k)
  }
}

object ChirpTest extends App {
  val data = Seq(
    Complex(1.23, -4.56),
    Complex(7.89,  0.12),
    Complex(-3.45, 6.78),
    Complex(9.01, -2.34),
    Complex(-5.67, 8.90),
    Complex(1.23,  4.56),
    Complex(-7.89, -0.12),
    Complex(3.45, -6.78),
    Complex(2.34,  3.21),
    Complex(-4.32, 1.09),
    Complex(0.0,  -2.34)
  )
  val ref = new FourierTransform(data)
  println(f"correct result = ${ref.compute()}")
  val chirp = new Chirp(data)
  val res = chirp.result
  println(f"chirp result = $res")
}
