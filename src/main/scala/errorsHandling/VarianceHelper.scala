package errorsHandling

object VarianceHelper {

    def calcMean(xs: Seq[Double]): Option[Double] = {
        if (xs.isEmpty) return None
        Some(xs.sum / xs.length)
    }

    /**
     * Implement the variance function in terms of flatMap. If the mean of a sequence is m,
     * the variance is the mean of math.pow(x - m, 2) for each element x in the sequence.
     * See the definition of variance on Wikipedia (http://mng.bz/0Qsr).
     * def variance(xs: Seq[Double]): Option[Double]
     *
     */
    def variance(xs: Seq[Double]): Option[Double] = {
        // flatMap ensures when calcMean yields None, it directly return None without doing the rest of computation
        calcMean(xs) flatMap (m => calcMean(xs.map(x => math.pow(x - m, 2))))
    }

    def variance2(xs: Seq[Double]): Option[Double] = {
        val mean = calcMean(xs)

        // "Classic paradigm" will be more complicated,
        // boilerplate code inspecting the Option[Double] mean: if valid / invalid ...
        mean match {
            case Some(meanValue) => calcMean(xs.map(x => math.pow(x - meanValue, 2)))
            case None => None
        }
    }
}
