package io.rob

import _root_.io.rob.ListsAndGCD._
import org.scalatest.{Matchers, WordSpec}

/**
 * Created by rob on 14/03/15.
 */
class ListsAndGCDTest extends WordSpec with Matchers {

  "My ListsAndGCD should" should {
    "find evaluate my input strings into individual numbers" in {

      val l1 = List("1", "2", "3", "2")
      val l2 = List("2", "2", "3", "3")

      evaluateInput(l1) should be (9)
      evaluateInput(l2) should be (108)
    }

    "My gcd should find the gcd of two numbers" in {
      gcd(6, 15) should be (3)
      gcd(49, 28) should be (7)
    }

    "My gcd should find gcd of a list of numbers" in {
      gcd(List(6, 12, 15)) should be (3)
      gcd(List(49, 28)) should be (7)
    }

    "Identify prime factors" in {
      primeFactors(6) should equal (List (2, 3))
    }

    "Fold over prime factors" in {
      formatPrimeFactors(List(2, 2, 2)) should equal (List(2, 3))
      formatPrimeFactors(List(2)) should equal (List(2, 1))
      formatPrimeFactors(List(2, 2, 2, 3)) should equal (List(2, 3, 3, 1))
    }

    "Go" in {
      go(List("2", "7 2", "2 2 7 1")) should equal ("7 1")
      go(List("4", "2 2 3 2 5 3", "3 2 5 3 11 1", "2 2 3 3 5 4 7 6 19 18", "3 10 5 15")) should equal ("3 2 5 3")
    }
  }

}
