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
        val random = SimpleRNG(444442)

        val tuples: immutable.Seq[(Double, RNG)] = Stream.fill(3)(random.double())

        lazy val headValue = tuples.head
        val allEqual = tuples.forall( v  => {
            println(v)
            v == headValue
        })

        assert(allEqual)
    }

    test("Integers generation") {
        val result = RandomUtils.ints(5)(SimpleRNG(42))
        val result2 = RandomUtils.ints2(5)(SimpleRNG(42))

        assertResult(result._1.reverse)(result2._1)

        println(result._1)
        println(result._2)
    }
}
