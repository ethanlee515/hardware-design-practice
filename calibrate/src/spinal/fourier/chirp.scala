package fourier

import complex._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import reference.fourier.ChirpPrecompute
import reference.fourier.FourierTransform
import spire.math.Complex
import spire.implicits._

class Chirp(xslen : Int) extends Component {
  val xs = in(Vec.fill(xslen)(Cx()))
  val ys = out(Vec.fill(xslen)(Cx()))
  val precompute = new ChirpPrecompute(xslen)
  val chirplen = precompute.chirplen
  val w = precompute.sqrt_primitive_root
  val cs = Vec.fill(chirplen)(Cx())
  for(i <- 0 until xslen) {
    val mul = new CxMul()
    mul.z1 := xs(i)
    mul.z2.setval(w ** (i * i))
    cs(i) := mul.prod
  }
  for(i <- xslen until chirplen) {
    cs(i).setval(Complex.zero[Double])
  }
  val ct = new CooleyTukey(chirplen)
  ct.xs := cs
  val gs_fft = Vec.fill(chirplen)(Cx())
  for(i <- 0 until chirplen) {
    val mul = new CxMul()
    mul.z1 := ct.ys(i)
    mul.z2.setval(precompute.vs_fft(i))
    gs_fft(i) := mul.prod
  }
  val ict = new InvCooleyTukey(chirplen)
  ict.xs := gs_fft
  for(i <- 0 until xslen) {
    val mul = new CxMul()
    mul.z1 := ict.ys(i)
    mul.z2.setval(w ** (i * i))
    ys(i) := mul.prod
  }
}

object TestChirp extends App {
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
  val ref = new reference.fourier.Chirp(data)
  println(f"correct result = ${ref.result}")
  SimConfig.compile { new Chirp(data.length) }.doSim { dut =>
    for(i <- 0 until data.length) {
      dut.xs(i).re #= data(i).real
      dut.xs(i).im #= data(i).imag
    }
    sleep(1)

/*
    val cs = Seq.tabulate(data.length) { i =>
      Complex(dut.cs(i).re.toDouble, dut.cs(i).im.toDouble)
    }
    println(f"cs = $cs")

    val gs_fft = Seq.tabulate(dut.chirplen) { i =>
      Complex(dut.gs_fft(i).re.toDouble, dut.gs_fft(i).im.toDouble)
    }
    println(f"gs-fft = $gs_fft")

    val gs = Seq.tabulate(dut.chirplen) { i =>
      Complex(dut.ict.ys(i).re.toDouble, dut.ict.ys(i).im.toDouble)
    }
    println(f"gs = $gs")
    */

    val res = Seq.tabulate(data.length) { i =>
      Complex(dut.ys(i).re.toDouble, dut.ys(i).im.toDouble)
    }
    println(f"computed result = $res")
  }
}
