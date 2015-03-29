package io.rob

trait RNG {
  def nextInt: (Int, RNG)
}
