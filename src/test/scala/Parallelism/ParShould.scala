package Parallelism

import java.util
import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

import fpinscala.parallelism.Par
import fpinscala.parallelism.Par.Par
import org.scalatest.FunSuite

class ParShould extends FunSuite{
    test("Can sort a integer list") {
        val parList = Par.unit(List(5,4,28,1,74))
        val sortedPar: Par[List[Int]] =  Par.sortPar(parList)
        // I DON'T KNOW HOW TO EXECUTE THIS, i.e. how to get a `ExecutorService`
    }

    test("Map a Par[A] => Par[B] with function f A=>B is equivalent to apply f on A then create a Parallel bloc"){
        val x = 42
        val f: Int=> Int = _*2
        val value: Par[Int] = Par.map(Par.unit(x))(f)

        assert( value == Par.unit(f(x)))
    }
}
