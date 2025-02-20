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
  val flipped = (x < 0)
  var xv = if(flipped) (-x) else x
  var yv = if(flipped) (-y) else y
  for(i <- 0 until 20) {
    val d = if (yv > 0) 1 else -1
    val x_new = xv + d * yv * (2.0 ** (-i))
    val y_new = yv - d * xv * (2.0 ** (-i))
    result += d * ATanPrecompute.get(i)
    xv = x_new
    yv = y_new
  } 
  if(flipped) {
    result += Pi
  }
  if(result < 0) {
    result += 2 * Pi
  }
}

object TestATan2 extends App {
  val x = -2.0 + 4.0 * Random.nextDouble()
  val y = -2.0 + 4.0 * Random.nextDouble()
  val theta = (new ATan2(y, x)).result
  val t = math.atan2(y, x)
  val ref_theta = if (t < 0) t + 2 * Pi else t
  println(s"reference atan2($y, $x) = $ref_theta")
  println(s"computed = $theta")
}
