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
//        assertResult(Stream(1))(stream.takeWhile(_ < 2)) // <= fail here
    }

    test("break as soon as found"){
        val stream = Stream(1,2,3,4,5,6)
        val exists = stream.exist(x => {
            println(x)
            x == 2
        })
        assert(exists, "2 does exist in the stream")
    }

    test("Break as soon as one element does not fit criteria"){
        val stream = Stream(1,2,3,4,5,6)
        val all = stream.forAll(x => {
            println(x)
            x > 2
        })
        assert(!all, "Not all element > 2")
    }

    test("Observe iteration order"){
        val stream = Stream(1,2,3,4,5,6)
        val all = stream.forAll(x => {
//            println(x)
            x  < 70
        })
        assert(all)
    }

    test("Break as soon as one element does not fit criteria - 2"){
        val stream = Stream(1,2,3,4,5,6)
        val all = stream.forAll2(x => {
            println(x)
            x > 2
        })
        assert(!all, "Not all element > 2")
    }

    test("head option") {
        val stream = Stream(1,2,3,4,5,6)
        assertResult(Some(1))(stream.headOption2())
    }

    test("head option yield nothing") {
        val stream = Stream.empty[Int]
        assertResult(None)(stream.headOption2())
    }

    test("append stream to stream") {
        val left = Stream(1,2)
        val right = Stream(3,4)

        assertResult(List(1,2,3,4))(left.append(right).toList2)
    }

    test("double Ints stream"){
        val doubled = Stream(1,2,3).map( 2*_)
        assertResult(List(2,4,6))(doubled.toList2)
    }

    test("filter"){
        val filtered= Stream(1,2,3).filter(_>1).toList2
        assertResult(List(2,3))(filtered)
    }

    test("flat map by right fold") {
        val stream= Stream(1,2,3).flatMapByRightFold(i => Stream(i, i))
        assertResult(Stream(1,1,2,2,3,3).toList2)(stream.toList2)
    }


    test("flat map by left fold") {
        val stream= Stream(1,2,3).flatMapByLeftFold(i => Stream(i, i))
        assertResult(Stream(1,1,2,2,3,3).toList2)(stream.toList2)
    }


    test("ONE iterations"){
        Stream(1,2,3,4).map((i) => {
            println("map " + i)
            i + 10
        }).filter(i => {
            println("filter " +  i)
            i % 2 == 0
        }).toList
    }

    test("Take 5 of an infinite stream"){
        assertResult(Stream(42, 42).toList2)(Stream.constantByUnfolding(42).take(2).toList2)
    }

    test("incrementing stream") {
        assertResult(List(42,43,44))(StreamExtensions.fromByUnfolding(42).take(3).toList2)
    }

    test("fibonacci stream"){
        assertResult(List(0,1,1,2,3,5,8))(StreamExtensions.fibByUnfolding2.take(7).toList2)
    }

   test("unfold"){
       val actual = Stream.unfold[Int, Int](1)(s => Option((s+1, s*2))).take(4).toList2
       assertResult(List(2,3,5,9))(actual)
   }

    test("Repeating integer '1'"){
        val actual = StreamExtensions.ones.take(3).toList2
        assertResult(List(1,1,1))(actual)
    }
}
