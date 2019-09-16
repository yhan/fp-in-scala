package strictness

object StrictSpikes {
    def maybeTwice(b: Boolean, i: => Int) = {
        if (b) i + i else 0
    }
//    maybeTwice: (b: Boolean, i: => Int)Int
}




