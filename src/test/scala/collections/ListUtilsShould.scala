package collections

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

    test("Filter list using flatMap") {
        val list = List[Int](1, 2, 3, 4)
        assertResult(List[Int](2, 4))(List.filterUsingFlatMap(list)(x => x % 2 == 0))
    }

    test("Project a list of A to a another list of B with projection function returning a list of B") {
        val flattenMap = List.flatMap(List(1, 2, 3))(i => List(i, i))
        assertResult(List[Int](1, 1, 2, 2, 3, 3))(flattenMap)
    }

    test("Specific Zip two list of Integer by summing each corresponding element") {
        val list1 = List(1, 2, 3)
        val list2 = List(4, 5, 6)

        assertResult(List(5, 7, 9))(List.addPairwise(list1, list2))
    }


    test("Zip two lists") {
        val list1 = List(1, 2, 3)
        val list2 = List(4, 5, 6)

        assertResult(List(5, 7, 9))(List.zipWith(list1, list2)((x, y) => x + y))
    }


    test("The second list is a sub sequence of the first list"){
        val list1 = collections.List("word1", "word2", "word2", "word3", "word1")
        val list2 = collections.List("word2", "word3")

        val isSebSequence = List.hasSubsequence(list1, list2)
        assert(isSebSequence)
    }

    test("The second list is NOT a sub sequence of the first list"){
        val list1 = collections.List("word1", "word2", "word2", "word3", "word1")
        val list2 = collections.List("word1", "word3")

        val isSebSequence = List.hasSubsequence(list1, list2)
        assert(!isSebSequence)
    }

    test("Nil list is subsequence of Nil list"){
        val list1 = List[String]()
        val list2 =  List[String]()

        val isSebSequence = List.hasSubsequence(list1 , list2)
        assert(isSebSequence)
    }

    def write(cmd: String): Any = {
        cmd match {
            case "start" | "go" => println("starting")
            case _ => println("doing nothing")
            case "stop" | "quit" | "exit" => println("stopping")
        }
    }

    test("multiple match cases"){
        write("stop")
    }
}
