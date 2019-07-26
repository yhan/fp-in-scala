import collections.List
import org.scalatest.FunSuite

class ListUtilsShould extends FunSuite {
    test("Can add 1 for each element of list") {
        val list = List[Int](1, 2, 3, 4)
        val incremented = List.incrementByOne(list)(_ + 1)
        assertResult(List[Int](2, 3, 4, 5))(incremented)
    }
}
