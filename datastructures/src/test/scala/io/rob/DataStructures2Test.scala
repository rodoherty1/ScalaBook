package io.rob

import io.rob.Datastructures._
import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.Checkers
import org.scalatest.{Matchers, WordSpec}

/**
 * Created by rob on 08/02/15.
 */
class DataStructures2Test extends WordSpec with Matchers with Checkers {

  "More fun with Datastructures" should {
    "Find max node in a Tree of Int" in {
      forAll(TreeGenerators.trees) { tree: Tree[Int] =>
        val max = maximum(tree)
        max >= Int.MinValue
      }.check
    }

    "Invoke treeMap on each Tree" in {
      forAll(TreeGenerators.trees) {tree: Tree[Int] =>
        treeMap(tree)(a => s"rob.$a")
        1 == 1
      }.check
    }

    "Invoke size using treeFold" in {
      forAll(TreeGenerators.trees) { tree: Tree[Int] =>
        fold(tree)(a => 1)((l, r) => 1 + l + r) == treeSize(tree)
      }.check
    }
    
    "Invoke maximum using treeFold" in {
      forAll(TreeGenerators.trees) { tree: Tree[Int] =>
        fold(tree)(a => a)((l, r) => l max r) == maximum(tree)
      }.check
    }
    
    "Invoke map using treeFold" in {
      forAll(TreeGenerators.trees) { tree: Tree[Int] =>
        fold(tree)(a => Leaf(s"rob.$a"): Tree[String])(Branch(_, _)) == treeMap(tree)(a => s"rob.$a")
      }.check
    }
  }
}
