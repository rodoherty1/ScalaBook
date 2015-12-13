package com.rob


/*
Basic properties of a Monoid

op(op(x, y), z) == op(x, op(y, z))
op(zero, x) == x

*/
trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

object Monoid {
  val stringMonoid = new Monoid[String] {
    override def op(a1: String, a2: String): String = {
      a1 + a2
      /*
      op("a", op("b", "c") ==? op(op("a", "b"), "c")
      "abc" == abc"
       */
    }

    override def zero: String = ""
  }

  val intMonoid = new Monoid[Int] {
    override def op(a:Int, b:Int) = {
      a + b
    }

    override def zero: Int = 1
  }

  val optionMonoid = new Monoid[Option[String]] {
    override def op(a: Option[String], b:Option[String]) = {
      a orElse b
    }

    override def zero: Option[String] = None
  }

  type Endo = String => String
  val endoMonoid = new Monoid[Endo] {
    override def op(a: Endo, b: Endo) = {
      a andThen b
    }

    override def zero: Endo = (a: String) => a
  }
}