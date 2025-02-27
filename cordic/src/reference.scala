package reference

import scala.math.sqrt
import scala.math.pow
import scala.math.atan
import scala.math.cos
import scala.math.sin
import scala.math.Pi

object Precompute {
  val iterations = 16
  val step_sizes = for (i <- 0 until iterations) yield atan(pow(2, -i)) / (Pi / 2)
  val correction = 0.607252935
}

class Cordic (theta : Double) {
  private var x = 1.0
  private var y = 1.0
  private var beta = theta - 0.5
  private var i = 1;
  
  def do_iterate {
    val sigma = if (beta > 0) 1 else -1
    val a = x / pow(2, i);
    val b = y / pow(2, i);
    // println(s"i = ${i}, beta = ${beta}")
    // println(s"i = ${i}, (x, y) = (${x}, ${y})")
    x = x - sigma * b;
    y = y + sigma * a;
    beta = beta - sigma * Precompute.step_sizes(i)
    i += 1
  }

  def compute_output = (Precompute.correction * x, Precompute.correction * y)
}

object Main extends App {
  if(args.length != 1) {
    println("usage: reference.Main inputvalue")
    sys.exit()
  }
  try {
    args(0).toDouble
  } catch {
    case e: NumberFormatException => {
      println("input not double")
      sys.exit()
    }
  }
  val v = args(0).toDouble
  if(v < 0 || v > 1) {
    println("input out of range")
    sys.exit()
  }

  val c = new Cordic(v)
  for(_ <- 2 to Precompute.iterations) {
    c.do_iterate
  }

  val out = c.compute_output

  println(f"Computed by Cordic: $out")
  println(f"Correct output: ${(cos(v * Pi / 2), sin(v * Pi / 2))}")
}
