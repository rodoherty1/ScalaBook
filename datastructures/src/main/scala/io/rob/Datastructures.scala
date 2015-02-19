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
    foldRight(as, Nil: List[B])((a, b) => f(a) ++ b)
  }

  def filter[A, B](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(a => if (f(a)) List(a) else Nil)
  }

  def zipInts(l1: List[Int], l2: List[Int]): List[Int] = {
    (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (x :: xs, y :: ys) => (x + y) :: zipInts(xs, ys)
    }
  }

  def zipWith[A, B](l1: List[A], l2: List[A])(f: (A, A) => B): List[B] = {
    (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (x :: xs, y :: ys) => f(x, y) :: zipWith(xs, ys)(f)
    }
  }

  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {

    def loop[A](l1: List[A], sub1: List[A]): Boolean = {
      (l1, sub1) match {
        case (_, Nil) => true
        case (Nil, _) => false
        case (x :: xs, y :: ys) => if (x == y) loop(xs, ys) else loop(xs, sub)
      }
    }
    loop(l, sub)
  }


  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](l: Tree[A], r: Tree[A]) extends Tree[A]

  def treeSize[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + treeSize(l) + treeSize(r)
    }
  }
  
  def maximum(tree: Tree[Int]): Int = {
    tree match {
      case Leaf(y) => y
      case Branch(l, r) => maximum(l) max maximum(r)
    }
  }
  
  def treeMap[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case Leaf(a) => Leaf(f(a))
      case Branch(l, r) => Branch(treeMap(l)(f), treeMap(r)(f))
    }
  }
  
  // See if you can define this function, then reimplement the functions you've already written for `Tree`.
  def fold[A,B](t: Tree[A])(l: A => B)(b: (B,B) => B): B = {
    ???
  }

}

