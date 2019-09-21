package state

import org.scalatest.FunSuite

import scala.collection.immutable

class StatefulAPIShould extends FunSuite {
    test("non negative random "){
        val random = SimpleRNG(42)

        val tuples: Stream[(Int, RNG)] = Stream.fill(3)(random.nonNegativeInt())

        lazy val headValue = tuples.head
        val allEqual = tuples.forall( v  => {
            println(v)
            v == headValue
        })

        assert(allEqual)
    }

    test("[0, 1) double"){
        val random = SimpleRNG(42)

        val tuples: immutable.Seq[(Double, RNG)] = Stream.fill(3)(random.double())

        lazy val headValue = tuples.head
        val allEqual = tuples.forall( v  => {
            println(v)
            v == headValue
        })

        assert(allEqual)
    }


    test("[0, 1) double - 2") {
        val (d, r) = RandomUtils.nextDouble(SimpleRNG(42))
        println(d, r)
    }

    test("Integers generation") {
//        val result = RandomUtils.ints(5)(SimpleRNG(42))
//        val result2 = RandomUtils.ints2(5)(SimpleRNG(42))
        val result3 = RandomUtils.ints3(5)(SimpleRNG(42))

//        assertResult(result._1.reverse)(result2._1)

        println(result3._1)
        println(result3._2)

//        println(result._1)
//        println(result._2)
    }

    test("Random non negative even integer "){
        val random = SimpleRNG(42)

        def iterate(r: RNG, count: Int):Unit = {
            if(count == 0) {
                return
            }
            val (a, rNext) = RandomUtils.nonNegativeEven(r)
            println(a, rNext)
            assert(a%2 == 0, "Should be even integer")
            assert(a>=0, "Should be positive or 0")

            iterate(rNext, count - 1)
        }

        iterate(random, 10)
    }

    test("use 'Combining state actions' to build intDouble "){
        println(RandomUtils.intDouble(SimpleRNG(42)))
        println(SimpleRNG(42).intDouble())

    }
}
