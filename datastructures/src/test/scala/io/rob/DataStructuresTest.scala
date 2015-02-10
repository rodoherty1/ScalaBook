package io.rob

import io.rob.Datastructures._
import org.scalacheck.Prop._
import org.scalatest.prop.Checkers
import org.scalatest.{Matchers, WordSpec}

/**
 * Created by rob on 08/02/15.
 */
class DataStructuresTest extends WordSpec with Matchers with Checkers {

  "Fun with foldRight" should {
    "Passing Nil and Cons into foldRight should always yield a non-zero lengthWithFoldRight list" in {
      val p = forAll { (l: List[Int]) =>
        l.foldRight(Nil: List[Int])(_ :: _).size == l.size
      }
      p.check
    }

    "Compute the length of a list using foldRight" in {
      forAll { (l: List[Int]) =>
        lengthWithFoldRight(l) == l.size
      }.check
    }

    "Compute the length of a list using foldLeft" in {
      forAll { (l: List[Int]) =>
        lengthWithFoldLeft(l) == l.size
      }.check
    }

    "Compute the sum of a list using foldLeft" in {
      forAll { (l: List[Int]) =>
        lengthWithFoldLeft(l) == l.size
      }.check
    }

    "Reverse a list using a foldLeft" in {
      forAll { (l: List[Int]) =>
        reverseListUsingFoldLeft(l) == l.reverse
      }.check
    }

    "Implement flatMap" in {
      forAll { (l: List[Int]) =>
        flatMap(l)((a: Int) => List[String](a.toString)) == l.flatMap(a => List[String](a.toString))
      }.check
    }

    "Homegrown foldRight should behave the same as the standard scala foldRight" in {
      forAll { (l: List[Int]) =>
        foldRight(l, 0)((a, b) => a - b) == l.foldRight(0)(_ - _)
      }.check
    }

    "Homegrown map should behave the same as the standard Scala map" in {
      forAll { (l: List[Int]) =>
        map(l)(_ + 1) == l.map(_ + 1)
      }.check
    }
  }
}
