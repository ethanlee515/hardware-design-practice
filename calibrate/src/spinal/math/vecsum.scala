package spinalmath

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spire.math.Complex
import spire.implicits._

class VecAdd(n: Int) extends Component {
  val xs = in(Vec.fill(n)(SFix(16 exp, -16 exp)))
  val s = out(SFix(16 exp, -16 exp))
  
  var terms = xs
  while(terms.size != 1) {
    val new_sz = (terms.size + 1) / 2
    val next_terms = Vec.fill(new_sz)(SFix(16 exp, -16 exp))
    for(j <- 0 until new_sz) {
      if(2 * j + 1 != terms.size) {
        next_terms(j) := terms(2 * j) + terms(2 * j + 1)
      } else {
        next_terms(j) := terms(2 * j)
      }
    }
    terms = next_terms
  }
  s := terms(0)
}

object TestVecAdd extends App {
  val n = scala.util.Random.nextInt(8) + 3
  def randd() : Double = {
    return -2 + 4 * scala.util.Random.nextDouble()
  }
  val arr = Seq.fill(n)(randd())
  val s = arr.reduce(_ + _)

  println(s"arr = $arr")
  println(s"sum = $s")

  SimConfig.compile{ new VecAdd(n) }.doSim { dut =>
    for(i <- 0 until n) {
      dut.xs(i) #= arr(i)
    }
    sleep(1)
    println(s"computed = ${dut.s.toDouble}")
  }
}
