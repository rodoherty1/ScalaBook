package io.rob

import scala.collection.parallel.immutable.ParSeq
import scala.io.Source

/**
 * Created by rob on 22/03/15.
 */
object LonelyInteger {


  def go(lines: List[String]): String = {
    val count = lines.head.toInt

    val arr: List[String] = lines.tail.head.split(" ").toList

    arr.foldLeft(List[String]())((acc, s) => {
      if (acc.contains(s)) {
        val index = acc.indexOf(s)
        acc.take(index) ++ acc.drop(index + 1)
      } else {
        s :: acc
      }
    }).head
  }

  def main(args: Array[String]) {
    println(go(Source.stdin.getLines().toList))
  }
}

