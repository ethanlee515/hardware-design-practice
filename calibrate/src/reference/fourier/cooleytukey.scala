package reference.fourier
import spire.math.Complex
import scala.math.Pi
import spire.implicits._

class Precompute(val xslen : Int) {
  val num_layers = 31 - Integer.numberOfLeadingZeros(xslen)

  def twiddle_factor(l : Int, j : Int) : Complex[Double] = {
    return Complex.polar(1.0, - 2 * Pi * j / (1 << (num_layers - l)))
  }

  def inv_twiddle_factor(l : Int, j : Int) : Complex[Double] = {
    return Complex.polar(1.0, 2 * Pi * j / (1 << (num_layers - l)))
  }
}

// Result index is bit-reversed
class CooleyTukey(xs : Seq[Complex[Double]]) {
  val precompute = new Precompute(xs.length)
  val num_layers = precompute.num_layers

  var content = xs
  var layer = 0

  def iter_layer() = {
    val group_size = xs.length >> layer
    val num_groups = 1 << layer
    val new_content = new Array[Complex[Double]](xs.length)
    for(g <- 0 until num_groups) {
      for(j <- 0 until (group_size / 2)) {
        val left = g * group_size + j
        val right = left + group_size / 2
        val vL = content(left)
        val vR = content(right)
        val aL = vL + vR
        val twiddle_factor = precompute.twiddle_factor(layer, j)
        val aR = (vL - vR) * twiddle_factor
        new_content(left) = aL
        new_content(right) = aR
      }
    }
    content = new_content
    layer = layer + 1
  }

  def compute() : Seq[Complex[Double]] = {
    for(_ <- 0 until num_layers) {
      iter_layer()
    }
    return content
  }
}

class InvCooleyTukey(xs : Seq[Complex[Double]]) {
  val precompute = new Precompute(xs.length)
  val num_layers = precompute.num_layers

  var content = xs
  var layer = num_layers - 1

  def iter_layer() = {
    val group_size = xs.length >> layer
    val num_groups = 1 << layer
    val new_content = new Array[Complex[Double]](xs.length)
    for(g <- 0 until num_groups) {
      for(j <- 0 until (group_size / 2)) {
        val left = g * group_size + j
        val right = left + group_size / 2
        val vL = content(left)
        val vR = content(right)
        val w = precompute.inv_twiddle_factor(layer, j)
        val aL = (vL + w * vR) / 2
        val aR = (vL - w * vR) / 2
        new_content(left) = aL
        new_content(right) = aR
      }
    }
    content = new_content
    layer = layer - 1
  }

  def compute() : Seq[Complex[Double]] = {
    for(_ <- 0 until num_layers) {
      iter_layer()
    }
    return content
  }
}

object TestCooleyTukey extends App {
  val data = Seq(
  Complex(1.23, -4.56),
  Complex(7.89, 0.12),
  Complex(-3.45, 6.78),
  Complex(9.01, -2.34),
  Complex(-5.67, 8.90),
  Complex(1.23, 4.56),
  Complex(-7.89, -0.12),
  Complex(3.45, -6.78)
  )
  println(data)
  val ct = new CooleyTukey(data)
  val res = ct.compute()
  println(res)
  val refres = (new FourierTransform(data)).compute()
  println(refres)
  val inv_ct = new InvCooleyTukey(res)
  val inv_res = inv_ct.compute()
  println(inv_res)
}
