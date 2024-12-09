# SpinalHDL Exercise: Implementing Cordic

Usage: `mill Main.runMain main.Main x` where `x` is a number in the interval [0, 1), for a normalized angle in [0, PI/2).

Details:
* 16 iterations, one clock cycle per iteration
* No pipelining yet, but uses ready/valid stream paradigm to take input and to indicate when computation is done
* Code quality kinda atrocious
