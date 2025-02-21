import spinal.core._
import spinal.core.sim._
import spinal.lib._
import fourier.Chirp
import scala.util.Random
import spire.math.Complex
import spire.implicits._

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
    val d = n / 2
    val theta_hat = dut.theta_hat.toDouble
    println(f"computed theta_hat = $theta_hat")
    val phi_hat = dut.phi_hat.toDouble
    println(f"computed phi_hat = $phi_hat")
  }
}
