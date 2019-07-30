import collections.List
import org.scalatest.FunSuite

class FoldShould extends FunSuite {

    test("Can circuit cut product") {
        val product = List.product2(List(1.2, 2, 5.8, 9.7, 0, 15, 42, 142))
        assertResult(0.0)(product)
    }

    /**
     * See what happens when you pass Nil and Cons themselves to foldRight, like this:
     * foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)).10 What do you think this
     * says about the relationship between foldRight and the data constructors of List?
     **/
    test("Can fold as constructor") {
        val list = List[Int](1, 2, 3)
        val actual = List.selfConstructor(list)

        assertResult(list)(actual)
    }

    /**
     * Compute the length of a list using foldRight.
     * def length[A](as: List[A]): Int
     **/
    test("Length of list") {
        val list = List[Int](1, 2, 3)
        val length = List.length(list)

        assertResult(3)(length)
    }

    test("Length of list using left fold") {
        val list = List[Int](1, 2, 3)
        val length = List.lengthLeftFold(list)

        assertResult(3)(length)
    }

    test("Sum list using left fold") {
        val list = List[Int](1, 2, 3)
        val sum = List.sumLeftFold(list)

        assertResult(6)(sum)
    }

    test("Product all int elements of list using left fold") {
        val list = List[Int](1, 2, 3, 4)
        val product = List.productLeftFold(list)

        assertResult(24)(product)
    }

    test("Reverse list") {
        val list = List[Int](1, 2, 3, 4)
        val reversed = List.reverse(list)

        assertResult(List[Int](4, 3, 2, 1))(reversed)
    }

    test("Append") {
        val first = List[Int](1, 2, 3, 4)
        val second = List[Int](9, 8, 7)

        assertResult(List[Int](1, 2, 3, 4, 9, 8, 7))(List.append(first, second))
    }

    test("Append using right fold") {
        val left = List[Int](1, 2, 3, 4)
        val right = List[Int](9, 8, 7)

        assertResult(List[Int](1, 2, 3, 4, 9, 8, 7))(List.appendByFold(left, right))
    }

    test("Flatten list of lists: right fold") {
        val l1 = List[Int](1, 2, 3, 4)
        val l2 = List[Int](9, 8, 7)
        val l3 = List[Int](42, 100, 300)
        val listOfLists = List[List[Int]](l1, l2, l3)

        assertResult(List[Int](1, 2, 3, 4, 9, 8, 7, 42, 100, 300))(List.flattenByRightFold(listOfLists))
    }

    test("Flatten list of lists: left fold") {
        val l1 = List[Int](1, 2, 3, 4)
        val l2 = List[Int](9, 8, 7)
        val l3 = List[Int](42, 100, 300)
        val listOfLists = List[List[Int]](l1, l2, l3)

        assertResult(List[Int](1, 2, 3, 4, 9, 8, 7, 42, 100, 300))(List.flattenByLeftFold(listOfLists))
    }
}


