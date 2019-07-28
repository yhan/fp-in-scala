import collections.List
import org.scalatest.FunSuite

class ListUtilsShould extends FunSuite {
    test("Can add 1 for each element of list") {
        val list = List[Int](1, 2, 3, 4)
        val incremented = List.map2(list)(_ + 1)
        assertResult(List[Int](2, 3, 4, 5))(incremented)
    }

    /**
     * Write a function that turns each value in a List[Double] into a String. You can use
     * the expression d.toString to convert some d: Double to a String.
     * EXERCISE 3.18
     **/
    test("Convert double to string") {
        val list = List[Double](1, 2, 3, 4)
        val asStrings = List[String]("1.0", "2.0", "3.0", "4.0")
        assertResult(asStrings)(List.map2(list)(convert = (x: Double) => x.toString))
    }

    /**
     * Write a function filter that removes elements from a list unless they satisfy a given
     * predicate. Use it to remove all odd numbers from a List[Int].
     * def filter[A](as: List[A])(f: A => Boolean): List[A]
     * EXERCISE 3.20
     **/
    test("Filter list") {
        val list = List[Int](1, 2, 3, 4)
        assertResult(List[Int](2, 4))(List.filter(list)(x => x % 2 == 0))
    }

    test("Project a list of A to a another list of B with projection function returning a list of B") {
        val flattenMap = List.flatMap(List(1, 2, 3))(i => List(i, i))
        assertResult(List[Int](1, 1, 2, 2, 3, 3))(flattenMap)
    }
}
