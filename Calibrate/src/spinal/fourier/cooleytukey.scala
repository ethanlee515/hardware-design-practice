package fourier

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import reference.fourier.Precompute
import reference.fourier.FourierTransform
import spire.math.Complex
import spinalmath._

class CooleyTukey(xslen : Int) extends Component {
  val precompute = new Precompute(xslen)
  val xs = in(Vec.fill(xslen)(Cx()))
  val ys = out(Vec.fill(xslen)(Cx()))

  var content = xs
  var layer = 0

  def iter_layer() = {
    val group_size = xslen >> layer
    val num_groups = 1 << layer
    var new_content = Vec.fill(xslen)(Cx())
    for(g <- 0 until num_groups) {
      for(j <- 0 until (group_size / 2)) {
        val left = g * group_size + j
        val right = left + group_size / 2
        val vL = content(left)
        val vR = content(right)
       
        val addL = new CxAdd()
        addL.z1 := vL
        addL.z2 := vR
        val aL = addL.s

        val subR = new CxSub()
        subR.z1 := vL
        subR.z2 := vR
        val twiddle_factor = precompute.twiddle_factor(layer, j)
        val mulR = new CxMul()
        mulR.z1 := subR.d
        mulR.z2.setval(twiddle_factor)
        val aR = mulR.prod

        new_content(left) := aL
        new_content(right) := aR
      }
    }
    content = new_content
    layer = layer + 1
  }
  
  for(_ <- 0 until precompute.num_layers) {
    iter_layer()
  }

  for(i <- 0 until xs.length) {
    ys(i) := content(i)
  }
}

class InvCooleyTukey(xslen : Int) extends Component {
  val precompute = new Precompute(xslen)
  val xs = in(Vec.fill(xslen)(Cx()))
  val ys = out(Vec.fill(xslen)(Cx()))

  var content = xs
  var layer = precompute.num_layers - 1

  def iter_layer() = {
    val group_size = xslen >> layer
    val num_groups = 1 << layer
    var new_content = Vec.fill(xslen)(Cx())
    for(g <- 0 until num_groups) {
      for(j <- 0 until (group_size / 2)) {
        val left = g * group_size + j
        val right = left + group_size / 2
        val vL = content(left)
        val vR = content(right)

        val w = precompute.inv_twiddle_factor(layer, j)

        val mul = new CxMul()
        mul.z1.setval(w)
        mul.z2 := vR
        
        val addL = new CxAdd()
        addL.z1 := vL
        addL.z2 := mul.prod
        val div2L = new CxDiv2()
        div2L.z := addL.s
        val aL = div2L.z2

        val subR = new CxSub()
        subR.z1 := vL
        subR.z2 := mul.prod
        val div2R = new CxDiv2()
        div2R.z := subR.d
        val aR = div2R.z2

        new_content(left) := aL
        new_content(right) := aR
      }
    }
    content = new_content
    layer = layer - 1
  }
  
  for(_ <- 0 until precompute.num_layers) {
    iter_layer()
  }

  for(i <- 0 until xs.length) {
    ys(i) := content(i)
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
  val ref_res = (new reference.fourier.CooleyTukey(data)).compute()
  println(f"correct result = $ref_res")
  SimConfig.compile { new CooleyTukey(data.length) }.doSim { dut =>
    for(i <- 0 until 8) {
      dut.xs(i).re #= data(i).real
      dut.xs(i).im #= data(i).imag
    }
    sleep(1)
    val res = Seq.tabulate(8) { i =>
      Complex(dut.ys(i).re.toDouble, dut.ys(i).im.toDouble)
    }
    println(f"computed result = $res")
  }
}

object TestInvCooleyTukey extends App {
  val data = Seq(
  Complex(1.23, -4.56),
  Complex(7.89, 0.12),
  Complex(-3.45, 6.78),
  Complex(9.01, -2.34),
  Complex(-5.67, 8.90),
  Complex(1.23, 4.56),
  Complex(-7.89, -0.12),
  Complex(3.45, -6.78),

  Complex(1.23, -4.56),
  Complex(7.89, 0.12),
  Complex(-3.45, 6.78),
  Complex(9.01, -2.34),
  Complex(-5.67, 8.90),
  Complex(1.23, 4.56),
  Complex(-7.89, -0.12),
  Complex(3.45, -6.78)
  )
  val ref_res = (new reference.fourier.InvCooleyTukey(data)).compute()
  println(f"correct result = $ref_res")
  SimConfig.compile { new InvCooleyTukey(data.length) }.doSim { dut =>
    for(i <- 0 until data.length) {
      dut.xs(i).re #= data(i).real
      dut.xs(i).im #= data(i).imag
    }
    sleep(1)
    val res = Seq.tabulate(data.length) { i =>
      Complex(dut.ys(i).re.toDouble, dut.ys(i).im.toDouble)
    }
    println(f"computed result = $res")
  }
}
