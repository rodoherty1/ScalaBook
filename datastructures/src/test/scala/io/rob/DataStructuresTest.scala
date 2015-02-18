package io.rob

import io.rob.Datastructures._
import org.scalacheck.{Gen, Arbitrary}
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
    
    "Implement filter using flatMap" in {
      forAll { (l: List[Int]) =>
        filter(l)(_ % 2 == 0) == l.filter(_ % 2 == 0)
      }.check
    }

    def g[T](implicit a: Arbitrary[T]) = for {
      v1 <- Arbitrary.arbitrary[List[T]]
      v2 <- Gen.containerOfN[List,T](v1.length, Arbitrary.arbitrary[T])
    } yield (v1,v2)
    
    "Implement zipIntegers" in {
      forAll(g[Int]) { case (l1, l2) =>
        zipInts(l1, l2).length == l1.length
      }.check
    }

    "Implement zipWith" in {
      forAll(g[Int]) { case (l1, l2) =>
        zipWith(l1, l2)(_ + _) == zipInts(l1, l2)
      }.check
    }
    
    "hasSubsequence" in {
      val l = List(1, 2, 3, 4)
      val sub1 = List(1, 2)
      val sub2 = List(2, 3)
      val sub3 = List(1, 3)

      hasSubsequence(l, sub1) should be (true)
      hasSubsequence(l, sub2) should be (true)
      hasSubsequence(l, sub3) should not be (true)
    }

  }
}
