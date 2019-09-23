import org.scalatest.FunSuite

class MiscTests extends FunSuite{
    test("Equivalent to sum"){
        println((1 to 5).reduceLeft(_ + _))
    }

    test("Currying"){
        val zscore2 =
            (mean: Int, sd: Int) =>
                (x: Int) =>
                    (x - mean) / sd

        println(zscore2(1, 10) (20) )
    }

    test("Invoke with _"){
        def zscore(mean: Int, sd: Int)(x: Int) = {

            println(x) //41
            println(mean) //7
            println(sd) // 4
            (x - mean) / sd
        }

        val normer: Int => Int =
//            zscore(7, 4) _
            zscore(7, 4)

        println(normer(41))
    }

    test("partial function") {
        val isEven: PartialFunction[Int, Boolean] = {
             case x if x % 2 == 0 => true
        }

        val isOdd: PartialFunction[Int, Boolean] = {
            case x if x % 2 != 0 => false
        }

        assertResult(Vector(true, true))((1 to 5) collect isEven)

        assertResult(Vector(false, true, false, true, false))((1 to 5) collect (isEven orElse isOdd))
        assertResult(Vector(false, true, false, true, false))((1 to 5) map (isEven orElse(isOdd)))
    }

    test ("partial function 2") {
        val isEven : PartialFunction[Int, Boolean] = {
            case x if x %2==0 => true
            case _ => false
        }
        assertResult(Vector(false, true))((1 to 2) collect isEven)
    }
}
