package io.rob

import org.scalatest.{WordSpec, Matchers}

/**
 * Created by rob on 05/03/15.
 */
class PrimeFactorsTest extends WordSpec with Matchers {

  "My awesome prime factor discovermotron" should {
    "discover prime factors" in {
      PrimeFactors(21) should be (List(3, 7))
    }
  }
}
