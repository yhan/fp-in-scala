import collections.List2
import org.scalatest.FunSuite

class FoldShould extends FunSuite{
  test("Can circuit cut product"){
    val product = List2.product2(List2(1.2, 2, 5.8, 9.7, 0, 15, 42, 142))
    assertResult(0.0)(product)
  }
}
