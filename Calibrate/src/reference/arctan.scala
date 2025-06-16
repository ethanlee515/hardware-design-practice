package reference

import scala.math.Pi
import spire.implicits._
import scala.util.Random

object ATanPrecompute {
  def get(i: Int) = {
    math.atan(2.0 ** (-i))
  }
}

class ATan2(y : Double, x : Double) {
  var result = 0.0
  val q2 = (x < 0 && y > 0)
  val q3 = (x < 0 && y < 0)
  var xv = if(q2 || q3) (-x) else x
  var yv = if(q2 || q3) (-y) else y
  for(i <- 0 until 20) {
    val d = if (yv > 0) 1 else -1
    val x_new = xv + d * yv * (2.0 ** (-i))
    val y_new = yv - d * xv * (2.0 ** (-i))
    result += d * ATanPrecompute.get(i)
    xv = x_new
    yv = y_new
  } 
  if(q2) {
    result += Pi
  }
  if(q3) {
    result -= Pi
  }
}

object TestATan2 extends App {
  val x = -2.0 + 4.0 * Random.nextDouble()
  val y = -2.0 + 4.0 * Random.nextDouble()
  val theta = (new ATan2(y, x)).result
  println(s"reference atan2($y, $x) = ${math.atan2(y, x)}")
  println(s"computed = $theta")
}
