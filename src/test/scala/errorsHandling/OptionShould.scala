package errorsHandling

import org.scalatest.FunSuite

    class OptionShould extends FunSuite {

        test("Can filter on Option which has value") {
            val filtered = Option(42).filter(x => x % 2 ==0)
            assertResult(Some(42))(filtered)
        }

        test("Can filter on Option which has NO value") {
            val function: Int => Boolean =x  => x % 2 == 0

            val filtered = None.filter(function)
            assertResult(None)(filtered)
        }

        test("Invalid map "){
           val mapped = Option(3).flatMap(x => {
               val i =  x % 2
               if(i == 0) Some("even") else None
           })

            assertResult(None)(mapped)
        }
    }

