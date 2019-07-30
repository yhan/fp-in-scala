import collections.{Branch, Leaf, Tree}
import org.scalatest.FunSuite

class TreeShould extends FunSuite {
    test("Size of Tree") {
        val tree = Branch(Leaf(1), Leaf(8))
        val size = collections.Tree.size(tree)

        assertResult(3)(size)
    }

    test("Size of Tree of 3 levels") {
        val tree = Branch(Branch(Leaf(1), Leaf(5)), Leaf(8))
        val size = collections.Tree.size(tree)

        assertResult(5)(size)
    }

    /**
     * Write a function maximum that returns the maximum element in a Tree[Int]. (Note:
     * In Scala, you can use x.max(y) or x max y to compute the maximum of two integers x
     * and y.)
     **/
    test("Find max value of tree") {
        val tree = Branch(Branch(Leaf(1), Leaf(5)), Leaf(8))
        val max = collections.Tree.maximum(tree)
        assertResult(8)(max)
    }

    /**
     * Write a function depth that returns the maximum path length from the root of a tree
     * to any leaf.
     * */
    test("Compute depth of tree"){
        val tree = Branch(Branch(Leaf(1), Leaf(5)), Leaf(8))
        val depth = Tree.depth(tree)
        assertResult(3)(depth)
    }
}
