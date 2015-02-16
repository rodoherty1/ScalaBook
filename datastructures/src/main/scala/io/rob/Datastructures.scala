package io.rob

/**
 * Created by rob on 09/02/15.
 */
object Datastructures {

  def lengthWithFoldRight[A](l: List[A]): Int = {
    l.foldRight(0)((_, b) => b + 1)
  }

  def lengthWithFoldLeft[A](l: List[A]): Int = {
    val a: Int = 0
    foldLeft(l, 0)((b, a) => b + 1)
  }


  def sumWithFoldLeft(l: List[Int]): Int = {
    foldLeft(l, 0)(_ + _)
  }

  def productWithFoldLeft(l: List[Int]): Int = {
    foldLeft(l, 1)(_ * _)
  }

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case h :: t => foldLeft(t, f(z, h))(f)
    }
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case h :: t => f(h, foldRight(t, z)(f))
    }
  }

  def map[A, B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, Nil: List[B])((a, b) => f(a) :: b)
  }


  def reverseListUsingFoldLeft[A](l: List[A]): List[A] = {
    l.foldLeft(List[A]())((b, a) => a :: b)
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight(as, Nil:List[B])((a, b) => f(a) ++ b)
  }
  
  def filter[A, B](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(a => if (f(a)) List(a) else Nil)
  }
  
  def zipInts[Int](l1: List[Int], l2: List[Int]): List[Int] = {
    ???
  }
}

