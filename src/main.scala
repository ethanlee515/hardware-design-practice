package main

import spinal.core._
import spinal.core.sim._
import spinal.lib._

import reference.Precompute

object Cordic extends Component {
  val step_sizes = Vec.fill(16)(AFix.U(1 exp, -16 exp))
  for(i <- 0 until 16) {
    step_sizes(i) := Precompute.step_sizes(i)
  }

  val theta = in(AFix.U(0 exp, -16 exp))
  val theta_valid = in(Bool())
  val cosine = out(AFix.U(0 exp, -16 exp))
  val sine = out(AFix.U(0 exp, -16 exp))

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
  val beta = Reg(AFix.S(1 exp, -16 exp))
  val half = AFix.U(1 exp, -16 exp)
  when ((state === STATE_WAITING) && theta_valid) {
      state := STATE_COMPUTING
      half := 0.5
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
  ccw_x := x - b
  val cw_x = AFix.U(1 exp, -16 exp)
  cw_x := x + b
  val next_x = AFix.U(1 exp, -16 exp)
  next_x := is_ccw ? ccw_x | cw_x

  val ccw_y = AFix.U(1 exp, -16 exp)
  ccw_y := y + a
  val cw_y = AFix.U(1 exp, -16 exp)
  cw_y := y - a
  val next_y = AFix.U(1 exp, -16 exp)
  next_y := is_ccw ? ccw_y | cw_y

  val step_size = AFix.S(1 exp, -16 exp)
  step_size := step_sizes(iter)
  val ccw_beta = AFix.S(1 exp, -16 exp)
  ccw_beta := beta - step_size 
  val cw_beta = AFix.S(1 exp, -16 exp)
  cw_beta := beta + step_size 
  beta := is_ccw ? ccw_beta | cw_beta

  val correction = AFix.U(0 exp, -16 exp)
  correction := Precompute.correction
  when(state === STATE_COMPUTING) {
    when (iter === 15) {
      state := STATE_DONE
      cosine := next_x * correction
      sine := next_y * correction
    } otherwise {
      x := next_x
      y := next_y
    }
    iter := iter + 1
  }
}

object Main extends App {
  SimConfig.compile { Cordic }.doSim { dut =>
    // clock-related boilerplate
    val cd = dut.clockDomain
    cd.forkStimulus(10)
    cd.waitSampling()
    cd.assertReset()
    cd.waitRisingEdge()
    cd.deassertReset()
    cd.waitSampling()
    
    // assert input
    dut.theta #= 0.4
    dut.theta_valid #= true;

    // compute
    sleep(500)
    println(s"sine = ${dut.sine.toDouble}, cosine = ${dut.cosine.toDouble}")
  }
}

