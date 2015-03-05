package io.rob

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{Matchers, WordSpec}
import org.scalatest.prop.Checkers
import org.scalacheck.Prop._


/**
 * Created by rob on 05/03/15.
 */
class MergeSortTest extends WordSpec with Checkers with Matchers {

  "My awesome MergeSort" should {
    "sort every cotton-pickin' list it gets its hand on" in {
      forAll { l: List[Int] =>
        val l1 = MergeSort(l)
        l1 == l.sorted
      }.check
    }
  }

}
