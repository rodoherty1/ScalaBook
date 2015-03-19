package io.rob

/**
 * Created by rob on 12/03/15.
 */
object Test1 {
  def findDiff (n: Int, nums: List[Int]): Int = {

    def solve(x0: Int, x1: Int, diff: Int) = {
      val minDiff = Math.min(x1 - x0, diff)
      if (minDiff < diff) x0 - minDiff else x0 + minDiff
    }


    def loop(xs: List[Int], diff: Int): Int = {
      xs match {
        case Nil => throw new IllegalStateException()
        case h :: t => if ((t.head - h) != diff) solve(h, t.head, diff) else loop (t, diff)
      }
    }

    loop (nums.tail, nums.tail.head - nums.head)
  }



}
