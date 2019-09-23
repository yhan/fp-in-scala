package Parallelism

import fpinscala.parallelism.Par
import fpinscala.parallelism.Par.Par
import org.scalatest.FunSuite

class ParShould extends FunSuite{
    test("Can sort a integer list") {
        val parList = Par.unit(List(5,4,28,1,74))
        val sortedPar: Par[List[Int]] =  Par.sortPar(parList)
        // I DON'T KNOW HOW TO EXECUTE THIS, i.e. how to get a `ExecutorService`
    }
}
