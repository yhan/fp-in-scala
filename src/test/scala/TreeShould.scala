import collections.{Branch, Leaf, Tree}
import org.scalatest.FunSuite



class TreeShould extends FunSuite {
    /**
     * 1
     * / \
     * 2   8
     * / \
     * 1   5
     **/
    val tree = Branch(3, Branch(2, Leaf(1), Leaf(5)), Leaf(8))

    test("Size of Tree") {
        val tree = Branch(2, Leaf(1), Leaf(8))

        val size = collections.Tree.size(tree)
        assertResult(3)(size)
    }

    test("Size of Tree of 3 levels") {
        val size = collections.Tree.size(tree)
        assertResult(5)(size)
    }

    test("Size of Tree of 3 levels (using fold)") {
        val size = collections.Tree.sizeByFold(tree)
        assertResult(5)(size)
    }

    /**
     * Write a function maximum that returns the maximum element in a Tree[Int]. (Note:
     * In Scala, you can use x.max(y) or x max y to compute the maximum of two integers x
     * and y.)
     **/
    test("Find max value of tree") {
        val max = collections.Tree.maximum(tree)
        assertResult(8)(max)
    }

    test("Max value of tree by fold") {
        val max = collections.Tree.maximumByFold(tree)
        assertResult(8)(max)
    }

    /**
     * Write a function depth that returns the maximum path length from the root of a tree
     * to any leaf.
     **/
    test("Depth of tree") {
        val depth = Tree.depth(tree)
        assertResult(3)(depth)
    }

    test("Depth of tree by fold") {
        val depth = Tree.depthByFold(tree)
        assertResult(3)(depth)
    }

    /**
     * Write a function map, analogous to the method of the same name on List, that modifies
     * each element in a tree with a given function
     **/
    test("Map") {
        val mapped = Tree.map(tree)(_ * 2)
        assertResult(Branch(6, Branch(4, Leaf(2), Leaf(10)), Leaf(16)))(mapped)
    }

    test("Map by fold") {
        val mapped = Tree.mapByFold(tree)(_ * 2)
        assertResult(Branch(6, Branch(4, Leaf(2), Leaf(10)), Leaf(16)))(mapped)
    }

    test("print all depth first") {
        Tree.printDepthFirst(tree)
    }
}
