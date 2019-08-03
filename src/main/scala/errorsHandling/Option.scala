package errorsHandling

object Option{
    /** An Option factory which creates Some(x) if the argument is not null,
     *  and None if it is null.
     *
     *  @param  x the value
     *  @return   Some(value) if value != null, None if value == null
     */
    def apply[A](x: A): Option[A] = if (x == null) None else Some(x)

    /** An Option factory which returns `None` in a manner consistent with
     *  the collections hierarchy.
     */
    def empty[A] : Option[A] = None
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]


trait Option[+A] {

    /** *
     * Apply f if the Option is not None.
     * */
    def map[B](f: A => B): Option[B] = this match {
        case None => None
        case Some(a) => Some(f(a))
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
        case None => None
        case Some(a) => ob
    }

    /**
     * returns the first Option if it’s defined; otherwise, it returns the second Option.
     **/
    def orElse[B >: A](ob: => Option[B]): Option[B] = {
        val value: Option[Some[A]] = this.map(Some(_))
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