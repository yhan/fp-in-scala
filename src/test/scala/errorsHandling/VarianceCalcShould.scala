package errorsHandling

import org.scalatest.FunSuite

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
