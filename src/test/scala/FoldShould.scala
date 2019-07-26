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

  test( "Sum list using left fold"){
    val list = List[Int](1, 2, 3)
    val sum = List.sumLeftFold(list)

    assertResult(6)(sum)
  }

  test( "Product all int elements of list using left fold"){
    val list = List[Int](1, 2, 3, 4)
    val product = List.productLeftFold(list)

    assertResult(24)(product)
  }

}
