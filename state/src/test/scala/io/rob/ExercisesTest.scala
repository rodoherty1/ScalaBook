package io.rob

import io.rob.Exercises._
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.Checkers
import org.scalatest.{Matchers, WordSpec}
import org.scalacheck.Prop._

/**
 * Created on 29/03/15.
 */
class ExercisesTest extends WordSpec with Matchers with Checkers {

  "My Simple Random Number Generator" should {
    "Generate non-negative integers" in {
      forAll {
        (rng: RNG) =>
          val (i, nextRNG) = nonNegativeInt(rng)
          i >= 0
      }.check
    }

    "Generate random doubles" in {
      forAll {
        (rng: RNG) =>
          val (d, nextRNG) = Exercises.double(rng)
          d >= 0 && d < 1
      }.check
    }

    "Generate pairs of doubles and ints" in {
      forAll {
        (rng: RNG) =>
          val (p, nextRNG) = Exercises.doubleInt(rng)
          p match {
            case (d, i) => (d >= 0 && d < 1) && (i < 0 || i > 0)
            case (_) => false
          }
      }
    }

    "Generate pairs of ints and doubles" in {
      forAll {
        (rng: RNG) =>
          val (p, nextRNG) = Exercises.intDouble(rng)
          p match {
            case (i, d) => (d >= 0 && d < 1) && (i < 0 || i > 0)
            case (_) => false
          }
      }
    }

    "Generate triplet of Doubles" in {
      forAll {
        (rng: RNG) =>
          val (d, nextRNG) = Exercises.double3(rng)
          d match {
            case (d1, d2, d3) => (d1 >= 0 && d1 < 1) &&
                                (d2 >= 0 && d2 < 1) &&
                                (d3 >= 0 && d3 < 1)
            case (_) => false
          }
      }
    }


  }

  lazy val genRNG: Gen[RNG] = {
    for {
        i <- arbitrary[Int]
    } yield SimpleRNG(i)
  }

  implicit lazy val arbRNG: Arbitrary[RNG] = Arbitrary(genRNG)



}
