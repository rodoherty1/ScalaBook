package io.rob

import scala.collection.immutable.IndexedSeq
import scala.collection.parallel.immutable.ParSeq
import scala.io.Source

object ListsAndGCD {

  def from(start: BigInt): Stream[BigInt] = start #:: from(start+1)

  val primes: Stream[BigInt] = Stream.cons(BigInt(2), from(BigInt(3)).filter(isPrime))

  def sqrt(number : BigInt): BigInt = {
    def next(n : BigInt, i : BigInt) : BigInt = (n + i/n) >> 1

    val one = BigInt(1)

    var n = one
    var n1 = next(n, number)

    while ((n1 - n).abs > one) {
      n = n1
      n1 = next(n, number)
    }

    while (n1 * n1 > number) {
      n1 -= one
    }

    n1
  }

  def isPrime(i: BigInt): Boolean = {
    val sqrtI = sqrt(i)
    primes.takeWhile(_ <= sqrtI).forall(i % _ > 0)
  }

  def evaluateInput(lines: List[String]): BigInt = {
    lines match {
      case Nil => 1
      case (x :: y :: xs) => BigInt(x).pow(y.toInt) * evaluateInput(xs)
      case List(_) => throw new IllegalArgumentException
    }
  }

  @annotation.tailrec
  def gcd(a: BigInt, b: BigInt): BigInt = if (b.equals(BigInt(0))) a else gcd(b, a % b)

  def gcd(xs: Seq[BigInt]): BigInt = {
    xs.foldLeft(xs)((xs, x) => {
      xs match {
        case Nil => throw new IllegalStateException
        case h :: Nil => xs
        case h :: t => gcd(h, t.head) :: t.tail
      }
    }).head
  }

  def primeFactors(num: BigInt): List[BigInt] = {
    def loop(n: BigInt, ps: Stream[BigInt]): List[BigInt] =
      if (isPrime(n)) List(n)
      else if (n.mod(ps.head).equals(BigInt(0))) ps.head :: loop(n / ps.head, ps)
      else loop(n, ps.tail)

    loop(num, primes)
  }

  def formatPrimeFactors(primeFactors: List[BigInt]) = {
    val result = primeFactors.foldRight((BigInt(0), BigInt(0), List[BigInt]()))((a, b) => {
      b match {
        case (h, cnt, acc) if BigInt(0).equals(h) => (a, BigInt(1), acc)
        case (h, cnt, acc) => if (h == a) (a, cnt + 1, acc) else (a, BigInt(1), h :: cnt :: acc)
        case _ => throw new IllegalArgumentException
      }
    })

    result match {
      case (h, count, acc) => h :: count :: acc
      case _ => throw new IllegalArgumentException
    }
  }

  def go(lines: List[String]): List[BigInt] = {
    val inputLists: List[List[String]] = lines.tail.map(_.split(" ").toList)

    val input: ParSeq[BigInt] = inputLists.par.map(evaluateInput)

    val gcdOfInput = gcd(input.toSeq.toList)

    val pf = primeFactors(gcdOfInput).sorted

    formatPrimeFactors(pf)
  }

  def main(args: Array[String]) {
    go(Source.stdin.getLines().toList).foreach(l => print(l + " "))
  }

}