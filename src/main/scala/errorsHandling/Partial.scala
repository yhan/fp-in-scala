//package errorsHandling
//
//trait Partial[Seq[E], A]{
////    def map2HandleAllErrors[EA >: E, EB >: E, B, C](b: Either[EB, B])(f: (A, B) => C) : Partial[Seq[E], C] = {
//////        val func = (aa: A) => b.map(bb => f(aa, bb))
//////        this.flatMap(func)
////
//////        (aa: A) => b.map(bb => f(aa, bb)
////
////    }
//
//
//    def map[B, EE <: E](f: A => B) : Partial[Seq[EE], B] = this match {
//        case Failure(errors) => {
//            val err: scala.Seq[Any] = errors
//            Failure(errors)
//        }
//        case Right(value) =>Success(f(value))
//    }
//
//}
//
//case class Failure[+E](errors: Seq[E]) extends Partial[Seq[E], Nothing]
//case class Success[A](value: A) extends Partial[Nothing, A]
//
//
//
