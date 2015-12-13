package com.rob

import org.scalacheck.{Prop, Properties}

object MonoidProperties extends Properties("Monoids") {

  import Monoid.intMonoid.{op => intOp}
  import Monoid.stringMonoid.{op => strOp}
  import Monoid.optionMonoid.{op => optOp}
  import Monoid.endoMonoid.{op => endoOp}
  import Monoid.Endo

  property("My StringMonoid should behave like a Monoid") =
    Prop.forAll {
      (a: String, b: String, c: String) =>
        strOp(strOp(a, b), c) == strOp(a, strOp(b, c))
    }


  property("My IntMonoid should behave like a Monoid") = {
    Prop.forAll {
      (a: Int, b: Int, c: Int) =>
        intOp(a, intOp(b, c)) == intOp(intOp(a, b), c)
    }
  }

  property("My OptionMonoid should behave like a Monoid") = {
    Prop.forAll {
      (a: Option[String], b: Option[String], c: Option[String]) =>
        optOp(a, optOp(b, c)) == optOp(optOp(a, b), c)
    }
  }

  property("My EndoMonoid shoud behave like a Monoid") = {
    Prop.forAll {
      // Create a generator for an EndoFunction !
    }
  }
}

