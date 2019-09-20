package state

case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
        val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
        val nextRNG = SimpleRNG(newSeed)
        val n = (newSeed >>> 16).toInt
        (n, nextRNG)
    }


    /*    Write a function to generate a Double between 0 and 1, not including 1. Note: You can
            use Int.MaxValue to obtain the maximum positive integer value, and you can use
            x.toDouble to convert an x: Int to a Double.*/
    def double(): (Double, RNG) = {
        val (int, nextRng) = this.nonNegativeInt()
        val i = if (int == Int.MaxValue) (int - 1) else int
        (i / Int.MaxValue.toDouble, nextRng)
    }

    def nonNegativeInt(): (Int, RNG) = {
        val (number, nextRng) = this.nextInt
        (if(number<0) {
            /*      Int.Min 's absolute value is 1 bigger than Int.Max; So when
            * we revert the sign, to be sure that we don't overflow we do `+1`   */
            (-number + 1)
        } else number, nextRng)
    }


    // We generate an integer >= 0 and divide it by one higher than the
    // maximum. This is just one possible solution.
    def double2(): (Double, RNG) = {
        val (i, r) = this.nonNegativeInt()
        (i / (Int.MaxValue.toDouble + 1), r)
    }

    def intDouble(): ((Int, Double), RNG) = {
        val (i, r) = this.nextInt
        val (d, r2) = r.double()

        ((i, d), r2)
    }

    def double3(rng: RNG): ((Double,Double,Double), RNG) = {
        val(d, r) = rng.double()
        val(d2, r2) = r.double()
        val(d3, r3) = r2.double()

        ((d, d2, d3), r3)
    }

    def doubleInt(): ((Double,Int), RNG)={
        val ((i,d), r) = this.intDouble()
        ((d, i), r)
    }



}
