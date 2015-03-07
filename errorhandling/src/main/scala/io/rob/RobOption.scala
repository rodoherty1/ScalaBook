package io.rob

/**
 * Created by rob on 07/03/15.
 */
sealed trait RobOption[+A] {
  def map[B](f: A => B): RobOption[B] = {
    this match {
      case RobNone => RobNone
      case RobSome(a) => RobSome(f(a))
    }
  }

  def getOrElse[B >: A](default: => B): B = {
    this match {
      case RobSome(a) => a
      case RobNone => default
    }
  }

  def orElse[B >: A](ob: => RobOption[B]): RobOption[B] = {
    (map(RobSome(_))) getOrElse ob
  }

  def flatMap[B](f: A => RobOption[B]): RobOption[B] = {
    map(f) getOrElse RobNone
  }

  def filter(f: A => Boolean): RobOption[A] = {
    flatMap(a => if (f(a)) RobSome(a) else RobNone)
  }
}

case class RobSome[+A] (get: A) extends RobOption[A]

object RobNone extends RobOption[Nothing]
