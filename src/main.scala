package main

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import scala.math.cos
import scala.math.sin
import scala.math.Pi

import reference.Precompute

class Cordic extends Component {
  val step_sizes = Vec.fill(16)(AFix.U(1 exp, -16 exp))
  for(i <- 0 until 16) {
    step_sizes(i) := Precompute.step_sizes(i)
  }

  val theta = in(AFix.U(0 exp, -16 exp))
  val theta_valid = in(Bool())

  val STATE_WAITING = 0
  val STATE_COMPUTING = 1
  val STATE_DONE = 2
  val state = Reg(UInt(2 bits)) init(STATE_WAITING)

  // This should be a stream or something
  val ready = out(Bool())
  ready := (state === STATE_WAITING)
  val valid = out(Bool())
  valid := (state === STATE_DONE)

  val x = Reg(AFix.U(1 exp, -16 exp)) init(1)
  val y = Reg(AFix.U(1 exp, -16 exp)) init(1)
  val correction = AFix.U(0 exp, -16 exp)
  correction := Precompute.correction
  val cosine = out(AFix.U(0 exp, -16 exp))
  val sine = out(AFix.U(0 exp, -16 exp))
  cosine := (x * correction).truncated
  sine := (y * correction).truncated

  val beta = Reg(AFix.S(1 exp, -16 exp))
  val half = AFix.U(1 exp, -16 exp)
  half := 0.5
  when ((state === STATE_WAITING) && theta_valid) {
    state := STATE_COMPUTING
    beta := theta - half
  } 

  val iter = Reg(UInt(4 bits)) init(1)
  val zero = AFix.S(1 exp, -16 exp)
  zero := 0.0
  val is_ccw = Bool()
  is_ccw := beta > zero

  val as = Vec.fill(16)(AFix.U(1 exp, -16 exp))
  val bs = Vec.fill(16)(AFix.U(1 exp, -16 exp))
  for (i <- 0 until 16) {
    as(i) := x >>| i
    bs(i) := y >>| i
  }
  val a = AFix.U(1 exp, -16 exp)
  val b = AFix.U(1 exp, -16 exp)
  a := as(iter)
  b := bs(iter)

  val ccw_x = AFix.U(1 exp, -16 exp)
  ccw_x := (x - b).truncated
  val cw_x = AFix.U(1 exp, -16 exp)
  cw_x := (x + b).truncated
  val next_x = AFix.U(1 exp, -16 exp)
  next_x := is_ccw ? ccw_x | cw_x

  val ccw_y = AFix.U(1 exp, -16 exp)
  ccw_y := (y + a).truncated
  val cw_y = AFix.U(1 exp, -16 exp)
  cw_y := (y - a).truncated
  val next_y = AFix.U(1 exp, -16 exp)
  next_y := is_ccw ? ccw_y | cw_y

  val step_size = step_sizes(iter)

  // turning these to output for debugging
  val ccw_beta = AFix.S(1 exp, -16 exp)
  ccw_beta := (beta - step_size).truncated
  val cw_beta = AFix.S(1 exp, -16 exp)
  cw_beta := (beta + step_size).truncated

  when(state === STATE_COMPUTING) {
    x := next_x
    y := next_y
    iter := iter + 1
    beta := is_ccw ? ccw_beta | cw_beta
  }

  // off by one error?
  when(iter === 15) {
    state := STATE_DONE
  }
}

object Main extends App {
  // parse command line input
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

  SimConfig.compile {
    val dut = new Cordic
    dut.iter.simPublic
    dut.beta.simPublic
    dut.x.simPublic
    dut.y.simPublic
    dut
  }.doSim { dut =>
    dut.theta_valid #= false

    // clock-related boilerplate
    val cd = dut.clockDomain
    cd.forkStimulus(10)
    cd.waitSampling()
    cd.assertReset()
    cd.waitRisingEdge()
    cd.deassertReset()
    cd.waitSampling()

    // wait some time for input
    sleep(100)

    // assert input
    dut.theta #= v
    dut.theta_valid #= true

    // compute
    for(i <- 1 to 20) {
      sleep(10)
      println(f"i = $i, iter = ${dut.iter.toInt}, beta = ${dut.beta.toDouble}")
      if(dut.valid.toBoolean) {
        println(f"Computed cosine = ${dut.cosine.toDouble}, sine = ${dut.sine.toDouble} after ${i} cycles")
        println(f"Correct cosine = ${cos(v * Pi / 2)}, sine = ${sin(v * Pi / 2)}")
        sys.exit()
      }
      //println(s"sine = ${dut.sine.toDouble}, cosine = ${dut.cosine.toDouble}")
      //println(s"valid = ${dut.valid.toBoolean}")
      //println(s"i = ${dut.iter.toInt}, beta = ${dut.beta.toDouble}")
      //println(s"i = ${dut.iter.toInt}, (x, y) = (${dut.x.toDouble}, ${dut.y.toDouble})")
    }
    // println(f"Computed cosine = ${dut.cosine.toDouble}, sine = ${dut.sine.toDouble}")
    //println("Something is wrong: no result after 20 cycles")
  }
}

