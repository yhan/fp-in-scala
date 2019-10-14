//package collections

import org.scalatest.FunSuite

import scala.annotation.tailrec

class ScalaCollectionShould extends FunSuite {
    test("Concat with ++") {
        val mainList = scala.collection.immutable.List(3, 2, 1)
        assertResult(List(3, 2, 1, 4, 5, 6))(mainList ++ List(4, 5, 6))

    }

    test("Prepend with ++:") {
        val mainList = scala.collection.immutable.List(3, 2, 1)
        assertResult(List(3, 2, 1, 4, 5, 6))(mainList ++: List(4, 5, 6))
        assertResult(List(3, 2, 1, 4, 5, 6))(mainList ::: List(4, 5, 6))
    }

    test("chain andThen") {
        val list = scala.collection.immutable.List(1, 2, 3, 4, 5, 6)
        val decrement: Int => Int = x => x - 1
        val doubleIt: Int => Int = x => x * 2
        val partialFunc: PartialFunction[Int, Int] = list andThen decrement andThen doubleIt
        assertResult(partialFunc(5))(10)
    }

    test("chain map") {
        val list = scala.collection.immutable.List(1, 2, 3, 4, 5, 6)
        val minus: Int => Int = x => x - 1
        val product: Int => Int = x => x * 2
        val newOne = list map minus map product
        assertResult(newOne)(List(0, 2, 4, 6, 8, 10))
    }

    test("extract value") {
        val list = List(1, 2, 3, 4, 5, 6)
        list match {
            case h :: h2 :: h3 :: tail => assert(h == 1)
                assert(h2 == 2)
                assert(h3 == 3)
                assertResult(List(4, 5, 6))(tail)
        }
    }

    test("Infinite list") {
        def fibFrom(a: Int, b: Int): List[Int] = a :: fibFrom(b, a + b)

        assertThrows[java.lang.StackOverflowError](fibFrom(1, 1))
    }

    test("Subset of an infinite stream") {
        def fibFrom(a: Int, b: Int): Stream[Int] = a #:: fibFrom(b, a + b)

        assertResult(List(1, 1, 2, 3, 5, 8, 13))(fibFrom(1, 1).take(7).toList)
    }

    test("Update Vector element via index?") {
        val vector = Vector(1, 2, 3, 4)
        val updated = vector.updated(3, 42)
        assertResult(42)(updated(3))
        assertResult(4)(vector(3)) //Immutable
    }

    test("left fold list") {
        val product: List[Int] = List(1, 1, 2, 3, 5, 8, 13).scanLeft(1)((y, x) => y * x)
        assertResult(1 * 1 * 2 * 3 * 5 * 8 * 13)(product.last)
    }
//
//    @tailrec final def iterate(list: List[Int], print: Int => Unit): Any = list match {
//        case ::(head, tl) => {
//            print(head)
//            iterate(tl, print)
//        }
//        case Nil =>
//    }

//
//    test("iterate on ") {
//        val print: Int => Unit = x => println(x)
//        iterate(list, print)
//    }

    val list = scala.collection.immutable.List(1, 2, 3, 4, 5, 6)

    test("what is the difference between collect and map ? ") {
        val donutAndPrices = List("Plain Donut", 1.5, "Strawberry Donut", 2.0, "Glazed Donut", 2.5)
        val names = donutAndPrices.collect { case name: String => name }

        assertResult(List("Plain Donut", "Strawberry Donut", "Glazed Donut"))(names)
    }

    test("cherry pick than filter and find first which satisfy a condition") {
        val donutAndPrices = List("Plain Donut", 1.5, "Strawberry Donut", 2.0, "Glazed Donut", 2.5)
        val option = donutAndPrices.collect { case price: Double => price }
        assertResult(2.5)(option.filter(x => x > 2.0).head)
    }
    /**
     * that
     * the sequence of elements to remove
     *
     * returns
     * a new sequence which contains all elements of this sequence except some of occurrences of elements that also appear in that.
     * If an element value x appears n times in that, then the first n occurrences of x will not form part of the result,
     * but any following occurrences will.
     **/
    test("diff") {
        val list1 = List("word1", "word2", "word2", "word3", "word1")
        val list2 = List("word1", "word4")
        val filtered = list1.diff(list2)

        assertResult(List("word2", "word2", "word3", "word1"))(filtered)
    }
}
