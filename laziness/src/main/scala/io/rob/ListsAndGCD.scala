package io.rob

import scala.collection.immutable.IndexedSeq
import scala.io.Source

/**
 * Created by rob on 13/03/15.
 */
object ListsAndGCD {

  def evaluateInput(lines: List[String]): Long = {
    lines match {
      case Nil => 1
      case (x :: y :: xs) => Math.pow(x.toInt, y.toInt).toInt * evaluateInput(xs)
      case List(_) => throw new IllegalArgumentException
    }
  }

  @annotation.tailrec
  def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)

  def gcd(xs: List[Long]): Long = {
    xs.foldLeft(xs)((xs, x) => {
      xs match {
        case Nil => throw new IllegalStateException
        case h :: Nil => xs
        case h :: t => gcd(h, t.head) :: t.tail
      }
    }).head
  }

  def isPrime(n: Long): Boolean = {
    if (n <= 2) true
    else {
      Stream(2, (n/2)-1).forall((n % _) > 0)
    }
  }

  def primeFactors(num: Long): List[Long] = {
    def loop(n: Long, ps: Stream[Long]): List[Long] =
      if (isPrime(n)) List(n)
      else if (n % ps.head == 0) ps.head :: loop(n / ps.head, ps)
      else loop(n, ps.tail)

    loop(num, Laziness.primes())
  }

  def formatPrimeFactors(primeFactors: List[Int]) = {
    // Can I foldRight over my prim factors and reformat List (2, 2, 2, 3) as "2 3 3 1 " (i.e. 2 ^ 3)
    val input = primeFactors.reverse
    val result = input.tail.foldRight((input.head, 1, List[Long]()))((a, b) => {
      b match {
        case (h, cnt, acc) => {
          if (h == a) (a, cnt+1, acc) else (a, 1, h :: cnt :: acc)
        }
        case _ => throw new IllegalArgumentException
      }
    })

    result match {
      case (h, count, acc) => {
        h :: count :: acc
      }
      case _ => throw new IllegalArgumentException
    }
  }

  def go(lines: List[String]): String = {
    val count = lines.head.toInt
    println(s"count: $count")
    val inputLists: List[List[String]] = lines.tail.map(_.split(" ").toList).toList

    val input: List[Long] = inputLists.map(evaluateInput)
    println(s"input: $input")

    val gcdOfInput = gcd(input)
    println(s"gcdOfInput: $gcdOfInput")

    val pf = primeFactors(gcdOfInput).sorted
    println(s"pf: $pf")
    formatPrimeFactors(pf).mkString(" ")
  }

  def main(args: Array[String]) {
    go(Source.stdin.getLines().toList)
  }

}