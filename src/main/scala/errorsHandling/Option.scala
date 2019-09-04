package errorsHandling

object Option{
    /** An Option factory which creates Some(x) if the argument is not null,
     *  and None if it is null.
     */
    def apply[A](x: A): Option[A] = if (x == null) None else Some(x)

    /** An Option factory which returns `None` in a manner consistent with
     *  the collections hierarchy.
     */
    def empty[A] : Option[A] = None
}

case class Some[+A](value: A) extends Option[A]

case object None extends Option[Nothing]

class IntegerOptionWrapper(intValue: Int){
   private val option = Option[Int](intValue)

    def absolute() : Option[Int] = {
        val function = option.lift(math.abs)
        function(option)
    }
}


object VarianceHelper{

    def calcMean(xs: Seq[Double]) : Option[Double] = {
        if(xs.isEmpty) return None
        Some(xs.sum/ xs.length)
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
            case Some(meanValue) =>  calcMean(xs.map(x => math.pow(x - meanValue, 2)))
            case None => None
        }
    }
}

trait Option[+A] {
    /**
     * Apply f if the Option is not None.
     * */
    def map[B](f: A => B): Option[B] = this match {
        case None => None
        case Some(a) => Some(f(a))
        case op => {
            throw new Exception("Other types than None or Some are not managed")
            }
    }

    def flatMap[B](f: A => Option[B]): Option[B] = {
        val value: Option[Option[B]] = map(f)
        value.getOrElse(None)
    }

    /**
     * explicit pattern matching
     **/
    def flatMap2[B](f: A => Option[B]): Option[B] = this match {
        case None => None
        case Some(a) => f(a)
    }

    def lift[A,B](f: A => B): Option[A] => Option[B] = {
        (_: Option[A]).map(f)
    }

    val absolute: Option[Double] => Option[Double] = {
        lift(f = math.abs)
    }

    /**
     * returns the result inside the Some case of the Option, or if the Option
     * is None, returns the given default value.
     **/
    def getOrElse[B >: A](default: => B): B = this match {
        case None => default
        case Some(a) => a
    }

    /**
     * explicit pattern matching
     **/
    def orElse2[B >: A](ob: => Option[B]): Option[B] = this match {
        case None => ob
        case _ => this
    }

    /**
     * returns the first Option if it’s defined; otherwise, it returns the second Option.
     **/
    def orElse[B >: A](ob: => Option[B]): Option[B] = {
        val value: Option[Some[A]] = this.map(f = (a: A) => Some(a))
        value.getOrElse(ob)
    }

    /**
     * Convert Some to None if
     * the value doesn’t satisfy f.
     **/
    def filter(f: A => Boolean): Option[A] = this match {
        case Some(a) if (f(a)) => this
        case _ => None
    }

    def filter2(f: A => Boolean): Option[A] = {
        this.flatMap(a => if (f(a)) Some(a) else None)
    }



}