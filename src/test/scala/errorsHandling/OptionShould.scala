package errorsHandling

import org.scalatest.FunSuite

class OptionShould extends FunSuite {

    test("Can filter on Option which has value") {
        val filtered = Option(42).filter(x => x % 2 == 0)
        assertResult(Some(42))(filtered)
    }

    test("Can filter on Option which has NO value") {
        val function: Int => Boolean = x => x % 2 == 0

        val filtered = None.filter(function)
        assertResult(None)(filtered)
    }

    test("Invalid map ") {
        val mapped = Option(3).flatMap(x => {
            val i = x % 2
            if (i == 0) Some("even") else None
        })

        assertResult(None)(mapped)
    }


    def mean(xs: Seq[Double]): Option[Double] = {
        if (xs.isEmpty) None
        Some(xs.sum / xs.length)
    }


    test("Variance with Option") {
        val seq: Seq[Double] = Seq()
        val variance = getVariance2(seq)

        assertResult(true)(variance.isInstanceOf[Some[Double]])
        assertResult(true)(variance.asInstanceOf[Some[Double]].value.isNaN)
    }

    def getVariance(seq: Seq[Double]): Double = {
        val mean = seq.sum / seq.length
        val variance: Double = seq.map(x => math.pow(x - mean, 2)).sum / seq.length
        variance
    }


    test("getVariance without Option") {
        val seq: Seq[Double] = Seq()
        val variance = getVariance(seq)

        assertResult(true)(variance.isNaN)
    }

    /**
     * Implement the variance function in terms of flatMap. If the mean of a sequence is m,
     * the variance is the mean of math.pow(x - m, 2) for each element x in the sequence.
     * See the definition of variance on Wikipedia (http://mng.bz/0Qsr).
     * def variance(xs: Seq[Double]): Option[Double]
     *
     */
    def getVariance2(xs: Seq[Double]): Option[Double] = {
        mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
    }

    test("absolute") {
        val optionInt = Option[Int](-42)
        val abs = optionInt.absolute(Option(-5))
        assertResult(5)(abs.asInstanceOf[Some[Double]].value)
    }

    test("absolute with instance function"){
        val option = new IntegerOptionWrapper(5)

        val abs = option.absolute()
        assertResult(5)(abs.asInstanceOf[Some[Int]].value)
    }
}


class VarianceCalcShould extends FunSuite {
    test("Normal case non functional impl"){
        val variance = VarianceHelper.variance2(for (i <- 1 to 6 ) yield i.toDouble)
        assertResult(2.9166666666666665)(variance.getOrElse())
    }

    test("edge case non functional impl"){
        val variance = VarianceHelper.variance2(Seq())
        assertResult(None)(variance)
    }

    test("Normal case in FP"){
        val variance = VarianceHelper.variance2(for (i <- 1 to 6 ) yield i.toDouble)
        assertResult(2.9166666666666665)(variance.getOrElse())
    }

    test("edge case in FP"){
        val variance = VarianceHelper.variance(Seq())
        assertResult(None)(variance)
    }
}

