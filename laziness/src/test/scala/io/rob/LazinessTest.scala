package io.rob

import io.rob.Laziness._
import org.scalatest.{Matchers, WordSpec}

/**
 * Created by rob on 10/03/15.
 */
class LazinessTest extends WordSpec with Matchers {

  "My Awesome Laziness Demo" should {
    "Create a constant stream" in {
      constant(1).take(5).toList should equal(List(1, 1, 1, 1, 1))
      constant('a').take(5).toList should equal(List('a', 'a', 'a', 'a', 'a'))
    }

    "Create an infinitely increasing stream" in {
      from(1).take(5).toList should equal(List(1, 2, 3, 4, 5))
    }

    "Create a Fibonacci series" in {
      fibs().take(8).toList should equal(List(0, 1, 1, 2, 3, 5, 8, 13))
    }

    "Create constant series via unfold" in {
      unfold(1)(a => Some((a, a))).take(5).toList should equal(constant(1).take(5).toList)
    }

    "Create fibs via unfold" in {
      unfold((0, 1)) { case ((x1, x2)) => Some((x1, (x2, x1 + x2)))}
    }

    "Map via unfold" in {
      map(Stream(1, 2, 3, 4))(_ + 1).toList should equal (List(1, 2, 3, 4) map (_ + 1))
    }

    "Take via Unfold" in {
      takeWhile(Stream(1, 2, 3, 4))(_ < 3).toList should equal (List(1, 2, 3, 4) takeWhile(_ < 3))
    }

    "Test1" in {
      Test1.findDiff(5, List(1, 3, 5, 9, 11)) should equal (7)
      Test1.findDiff(5, List(1, 5, 7, 9, 11)) should equal (3)
    }

    "Primes" in {
      primes().take(5).toList should equal (List(2, 3, 5, 7, 11))
    }
  }

}
