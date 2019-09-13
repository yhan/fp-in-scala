package errorsHandling

import org.scalatest.FunSuite

class EitherShould extends FunSuite {
    test("map2 yields right") {
        val a = Right(0)
        val b = Right(42)
        assertResult(Right(0))(a.map2(b)((x, y) => x/y))
    }

    test("map2 yields error") {
        val a = Right(42)
        val b = Right(0)
        intercept[ArithmeticException]{
            a.map2[ArithmeticException, Int, Int](b)((x, y) => x/y)
        }  // => on pert l'interet d'Either. Il faut trouver un moyen d'encapsuler Exception

//        val result = Either.Try(a.map2(b)((x, y) => x/y))
//        assertResult(Left())(result)
    }
}
