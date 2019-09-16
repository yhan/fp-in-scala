package strictness

sealed trait Stream[+A]{
    def toList: List[A]  = this match {
        case Empty => List()
        case Cons(h, t) => h() :: t().toList
    }

    def toList2 : List[A] = {
        @scala.annotation.tailrec def walk(s: Stream[A], accumulator : List[A]): List[A] = s match {
            case Empty => accumulator
            case Cons(h, t) => walk(t (), h():: accumulator)
        }

        walk(this, List()).reverse
    }

    def take(n: Int): Stream[A] = this match {
        case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n - 1))
        case Cons(h, _) if n == 1 => Stream.cons(h(), Stream.empty)
        case _ => Stream.empty
    }


//    def take2(n: Int): Stream[A] = {
//        def walk(s : Stream[A], step : Int, acc: Stream[A]) : Stream[A] = s match {
//            case Empty => Empty
//            case Cons(h, t) =>  walk(t(), step - 1,  )
//        }
//    }

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
    /*
    The toto: => B type annotation.
     indicates that the argument is of type B, but won’t be
evaluated until it’s needed by the function
     */
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
        lazy val head = hd
        lazy val tail = tl
        Cons(() => head, () => tail)
    }
    def empty[A]: Stream[A] = Empty
    def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}


