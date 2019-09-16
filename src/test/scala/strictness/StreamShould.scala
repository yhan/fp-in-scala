package strictness

import org.scalatest.FunSuite

class StreamShould extends FunSuite{
    test("build list from stream") {
        val stream = Stream(1,2,3,4,5,6)
        assertResult(List(1,2,3,4,5,6))(stream.toList2)
    }

    test("take n elements"){
        val stream = Stream(1,2,3,4,5,6)
//        assertResult(Stream(1, 2))(stream.take(2))

        print(stream.take(2).toList2)
    }
}
