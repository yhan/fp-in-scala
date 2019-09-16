package strictness

import org.scalatest.FunSuite

class StreamShould extends FunSuite{
    test("build list from stream") {
        val stream = Stream(1,2,3,4,5,6)
        assertResult(List(1,2,3,4,5,6))(stream.toList2)
    }

    test("take n elements"){
        val stream = Stream(1,2,3,4,5,6)
        assertResult(stream.take(2).toList2)(List(1,2))
    }

    test("take while"){
        val stream = Stream(1,2,3,4,5,6)
        assertResult(stream.takeWhile(_ < 2).toList2)(List(1))

        assertResult(Stream(1))(stream.takeWhile(_ < 2)) // <= fail here
    }


    test("break as soon as found"){
        val stream = Stream(1,2,3,4,5,6)
        val exists = stream.exist2(x => {
            println(x)
            x == 2
        })

        assert(exists, "2 does exist in the stream")
    }
}
