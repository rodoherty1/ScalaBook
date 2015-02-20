package io.rob

import io.rob.Datastructures._
import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.Checkers
import org.scalatest.{Matchers, WordSpec}

/**
 * Created by rob on 08/02/15.
 */
object TreeGenerators {
    lazy val leafs: Gen[Leaf[Int]] = for {
      i <- Arbitrary.arbitrary[Int]
    } yield Leaf(i)

    lazy val branches: Gen[Branch[Int]] = for {
      l <- trees
      r <- trees
    } yield Branch(l, r)

    lazy val trees: Gen[Tree[Int]] = for {
      isLeaf <- Gen.oneOf(true, false)
      tree <- if (isLeaf) leafs else branches
    } yield tree

    implicit lazy val treeGenerator: Arbitrary[Tree[Int]] = Arbitrary(trees)
}
