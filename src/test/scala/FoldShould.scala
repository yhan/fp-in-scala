import collections.List2
import org.scalatest.FunSuite

class FoldShould extends FunSuite {

  test("Can circuit cut product") {
    val product = List2.product2(List2(1.2, 2, 5.8, 9.7, 0, 15, 42, 142))
    assertResult(0.0)(product)
  }

  /**
   * See what happens when you pass Nil and Cons themselves to foldRight, like this:
   * foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)).10 What do you think this
   * says about the relationship between foldRight and the data constructors of List?
   **/
  test("Can fold as constructor") {
    val list = List2[Int](1, 2, 3)
    val actual = List2.selfConstructor(list)

    assertResult(list)(actual)
  }
}
