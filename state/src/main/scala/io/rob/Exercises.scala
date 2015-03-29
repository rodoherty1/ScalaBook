package io.rob

/**
 * Created on 29/03/15.
 */
object Exercises {

  def nonNegativeInt(rng: RNG) : (Int, RNG) = {
    val (a, nextRNG): (Int, RNG) = rng.nextInt
    (Math.abs(a), nextRNG)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, nextRNG) = nonNegativeInt(rng)
    (i / Int.MaxValue, nextRNG)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, rng1) = double(rng)
    val (i, rng2) = rng1.nextInt
    ((d, i), rng2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val ((d, i), rng1) = doubleInt(rng)
    ((i, d), rng1)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng)
    val (d3, rng3) = double(rng)
    ((d1, d2, d3), rng3)
  }

}
