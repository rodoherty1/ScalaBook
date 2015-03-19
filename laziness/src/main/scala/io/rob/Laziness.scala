package io.rob

/**
 * Created by rob on 10/03/15.
 */
object Laziness {

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = a #:: constant(a)

  def from(n: Int): Stream[Int] = n #:: from(n + 1)

  def fibs(): Stream[Int] = {
    def loop(x1: Int, x2: Int): Stream[Int] = {
      Stream.cons(x1, loop(x2, x1 + x2))
    }

    loop(0, 1)
  }

  def primes(): Stream[Long] = {
      unfold(2)(s => {
        val a = from(s).dropWhile(x => (2 until x).exists(x % _ == 0)).head
        Some (a, a+1)
      })
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => Stream.empty
      case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    }
  }

  def map[A, B](l: Stream[A])(f: A => B): Stream[B] = {
    unfold(l) {
      case (Stream.cons(x, xs)) => Some((f(x), xs))
      case empty => None
    }
  }

  def takeWhile[A](l: Stream[A])(f: A => Boolean): Stream[A] = {
    unfold(l) {
      case (Stream.cons(x, xs)) if f(x) => Some((x, xs))
      case _ => None
    }
  }

}
