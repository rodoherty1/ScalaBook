package io.rob

import org.scalatest.{WordSpec, Matchers, FunSuite}

/**
 * Created by rob on 22/03/15.
 */
class LonelyIntegerTest extends WordSpec with Matchers {

  "My LonelyInteger app" should {
    "find the lonely integer" in {
      LonelyInteger.go(List("3", "1 2 1")) should equal ("2")
      LonelyInteger.go(List("9", "4 9 95 93 57 4 57 93 9")) should equal ("95")

    }
  }

}
