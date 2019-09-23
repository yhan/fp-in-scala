package errorsHandling



/****
    Structure represents either A or B
*****/
trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
        case Left(e) => Left(e)
        case Right(a) => Right(f(a))
    }

    /*The B >: A says
    that the B type
    parameter must be
    a supertype of A.  **Here, a is subtype of A**
     */
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
        case Left(e) => Left(e)
        case Right(a) => f(a)
    }

    /** ': =>' indicates that the argument is of type B, but won’t be
     * evaluated until it’s needed by the function.      */
    def orElse[EE >: E, AA >: A](b: => Either[EE, AA]): Either[EE, AA] = this match {
        case Left(_) => b
        case Right(a) => Right(a)
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
            val func = (aa: A) => {
                val mapped = b.map(bb => f(aa, bb))
                mapped
            }
            this.flatMap(func)
    }

    def map2_2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
        for {bb <- b; // map
             aa <- this // flatMap
        } yield f(aa, bb)
    }
}

object Either {
    def Try[A](a: => A): Either[Exception, A] = {
        try Right(a) catch {
            case e: Exception => Left(e)
        }
    }

//    //These should return the first error  that’s encountered, if there is one.
//    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
//        es.foldRight[Either[E, List[A]]](Right(Nil))((h, t) => h.map2(t)(_ :: _))
//    }

    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
        case Nil => Right(Nil)
        case ::(head, tl) => head.flatMap(h => sequence(tl).map(h :: _))
    }

//    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
//        as.foldRight[Either[E, List[B]]](Right(Nil))((h, r) => f(h).map2(r)(_ :: _))
//    }

    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
        case Nil => Right(Nil)
        case ::(head, tl) => f(head).map2(traverse(tl)(f))(_::_)
    }

}


case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]









