package io.rob

/**
 * Created by rob on 05/03/15.
 */
object PrimeFactors {

  def apply(num: Int): List[Int] = {
    def loop(curr: Int, l: List[Int]): List[Int] = {
      println (s"curr: $curr, l: $l")
      for (n <- 2 to curr if curr % n == 0) {
        return loop(curr / n, l :+ n)
      }
      l
    }
    loop(num, Nil)
  }

}
