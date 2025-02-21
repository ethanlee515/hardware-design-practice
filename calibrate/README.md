# Calibrate

Implementing Algorithm 1 from page 9 of [this](https://arxiv.org/pdf/2407.01583) paper.

## Project structure

* `reference`: Scala implementation; treated as the golden model.
* `spinal`: Our SpinalHDL implementation.
  * `fourier`: contains the "Chirp" algorithm for optimized Fourier transform on arbitrary-length sequences.
    Calls the "Cooley-Tukey" FFT algorithm as a subroutine.
  * `math`: Contains miscellaneous mathematical components.
    * `arctan`: CORDIC-based implementation of atan2
    * `complex`: Complex numbers
    * `sqrt`: Newton's method implemenetation for square root
    * `vecsum`: Summing a vector using a binary tree

## Issues and Questions

Though we now output the correct result, there are still lots to work on.
* Maybe most egregiously, the entire calculation is done in a single clock cycle.
  This kills the clock rate.
  How do I know what is my current critical path length though, and how many stages should I cut my circuit into?
* Everything is 32-bit precision fixed-point number, with 16 bits of integer part and 16 bits of fractional part.
  Maybe the precision does not need to be this high.
  Perhaps a lot of the quantities do not require an integer part either.
* There might be a better way to do a square root than by Newton's method.
  On the same topic, I don't know how many iterations of Newton's method to run, but 30 is probably overkill.
* Maybe I should be borrowing my teammates' complex number type instead of rolling my own?
