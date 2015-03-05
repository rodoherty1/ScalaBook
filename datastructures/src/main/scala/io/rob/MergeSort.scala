package io.rob

/**
 * Created by rob on 05/03/15.
 */
object MergeSort {

  def apply(l: List[Int]): List[Int] = {
    val mid = l.length / 2;
    if (mid == 0) l
    else {
      val left = l.take(mid)
      val right = l.drop(mid)
      merge(MergeSort(left), MergeSort(right))
    }
  }

  def merge (left: List[Int], right: List[Int]): List[Int] = {
    (left, right) match {
      case (xs, Nil) => xs
      case (Nil, xs) => xs
      case (l :: ls, r :: rs) => if (l < r) l :: merge(ls, right) else r :: merge (left, rs)
    }
  }
}
