package errorsHandling


case class Some[+A](value: A) extends Option[A]

case object None extends Option[Nothing]

trait Option[+A] {

    /**
     * Apply f if the Option is not None.
     **/
    def map[B](f: A => B): Option[B] = this match {
        case None => None
        case Some(a) => Some(f(a))
        case op => {
            throw new Exception("Other types than None or Some are not managed")
        }
    }

    /**
     * explicit pattern matching
     **/
    def flatMap2[B](f: A => Option[B]): Option[B] = this match {
        case None => None
        case Some(a) => f(a)
    }

    def flatMap[B](f: A => Option[B]): Option[B] = {
        val value: Option[Option[B]] = map(f)
        value.getOrElse(None)
    }

    def lift[A, B](f: A => B): Option[A] => Option[B] = {
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

object Option {
    /** An Option factory which creates Some(x) if the argument is not null,
     * and None if it is null. */
    def apply[A](x: A): Option[A] = if (x == null) None else Some(x)

    /** An Option factory which returns `None` in a manner consistent with
     * the collections hierarchy. */
    def empty[A]: Option[A] = None

    /** ': =>' indicates that the argument is of type B, but won’t be
        evaluated until it’s needed by the function.      */
    def Try[A](a : => A) : Option[A] = {
        try{Some(a)} catch {case e:Exception => None}
    }

    /*
   Combines a list of Options into one Option containing
        a list of all the Some values in the original list. If the original list contains None even
    once, the result of the function should be None; otherwise the result should be Some
    with a list of all the values.
 */
    def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
        case Nil => None
        case head :: tl => head.flatMap((hh: A) => {
            val value: Option[List[A]] = sequence(tl)
            value.map(hh :: _)
        })
    }


    /** **********
     * It can also be implemented using `foldRight` and `map2`. The type annotation on `foldRight` is needed here; otherwise
     * Scala wrongly infers the result type of the fold as `Some[Nil.type]` and reports a type error (try it!). This is an
     * unfortunate consequence of Scala using subtyping to encode algebraic data types.
     * // override def foldRight[B](z: B)(op: (A, B) => B): B
     * *********/
    def sequence_1[A](a: List[Option[A]]): Option[List[A]] = {
        a.foldRight[Option[List[A]]](Some(Nil))((head, tail) => map2(head, tail)(_ :: _))
    }

    /** *
     * Lift binary arguments func to two option argument func
     * ***/
    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
        val func = (aa: A) => {
            val value: Option[C] = b map (bb => f(aa, bb))
            value
        }
        a flatMap func
    }


    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
        case Nil => None
        case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
    }


    def traverse2[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
        a.foldRight[Option[List[B]]](Some(Nil))((h: A, result: Option[List[B]]) => map2(f(h), result)(_ :: _))
    }



    def liftTriple[A, B, C, D](a: Option[A], b: Option[B], c: Option[C])(f: (A, B, C) => D): Option[D] = {
        val func: (A, B) => Option[D] = (aa: A, bb: B) => {
            val value: Option[D] = c map (cc => f(aa, bb, cc))
            value
        }
        return map2(a, b)(func).getOrElse(None)
    }

    def sequenceByTraverse[A](a: List[Option[A]]) : Option[List[A]] ={
        traverse2(a)(x => x)
    }


}



