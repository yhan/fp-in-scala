package errorsHandling


trait Option2[+A] {
//
//
//    def apply[A](x: A): Option2[A] = if (x == null) None2 else Some2(x)
//
//    /** An Option factory which returns `None` in a manner consistent with
//     *  the collections hierarchy.
//     */
//    def empty[A] : Option2[A] = None2


    case class Some2[+A](get: A) extends Option2[A]

    case object None2 extends Option2[Nothing]

    /** *
     * Apply f if the Option is not None.
     * */
    def map[B](f: A => B): Option2[B] = this match {
        case None2 => None2
        case Some2(a) => Some2(f(a))
    }

    def flatMap[B](f: A => Option2[B]): Option2[B] = {
        val value: Option2[Option2[B]] = map(f)
        value.getOrElse(None2)
    }

    /**
     * explicit pattern matching
     **/
    def flatMap2[B](f: A => Option2[B]): Option2[B] = this match {
        case None2 => None2
        case Some2(a) => f(a)
    }

    /**
     * returns the result inside the Some case of the Option, or if the Option
     * is None, returns the given default value.
     **/
    def getOrElse[B >: A](default: => B): B = this match {
        case None2 => default
        case Some2(a) => a
    }

    /**
     * explicit pattern matching
     **/
    def orElse2[B >: A](ob: => Option2[B]): Option2[B] = this match {
        case None2 => None2
        case Some2(a) => ob
    }

    /**
     * returns the first Option if it’s defined; otherwise, it returns the second Option.
     **/
    def orElse[B >: A](ob: => Option2[B]): Option2[B] = {
        val value: Option2[Some2[A]] = this.map(Some2(_))
        value.getOrElse(ob)
    }

    /**
     * Convert Some to None if
     * the value doesn’t satisfy f.
     **/
    def filter(f: A => Boolean): Option2[A] = this match {
        case Some2(a) if (f(a)) => this
        case _ => None2
    }

    def filter2(f: A => Boolean): Option2[A] = {
        this.flatMap(a => if (f(a)) Some2(a) else None2)
    }
}
//
//
//object Option2{
//    def apply[A](a: A): Option2[A] = {
//        None2
//    }
//
//    def hello(): Unit ={
//       Option2(2)
//   }
//}



