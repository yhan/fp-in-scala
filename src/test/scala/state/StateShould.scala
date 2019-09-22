package state

import org.scalatest.FunSuite
import State._

class StateShould extends FunSuite{
    test("Propagate state") {
        //case class State[S, +A](run: S => (A, S))
        val start = State[String, Int]((s: String) => (s.length, " toto" + s))
        val t: (Int, String) = start.map(i => i * 2 )
            .run("?")

        println(t._2)
        val t2 = start.run(t._2)

        println(t2)
    }

    test("Generate integer list"){
        println(State.ints(9))
    }

    test("Produce random list of integer than calculate the modulo eachIntegerInTheList %  given int"){
        val randInt:  Rand[Int] =  State.unit(100)
        println(State.nsSugar(randInt).run(SimpleRNG(42)))
    }

    test("Produce random list of integer than calculate the modulo eachIntegerInTheList %  given int - 2"){
        val randInt:  Rand[Int] =  State.unit(100)
        println(State.ns2(randInt).run(SimpleRNG(42)))
    }
}
