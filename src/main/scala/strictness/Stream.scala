package strictness

sealed trait Stream[+A] {
    def toList: List[A] = this match {
        case Empty => List()
        case Cons(h, t) => h() :: t().toList
    }

    def toList2: List[A] = {
        @scala.annotation.tailrec def walk(s: Stream[A], accumulator: List[A]): List[A] = s match {
            case Empty => accumulator
            case Cons(h, t) => walk(t(), h() :: accumulator)
        }

        walk(this, List()).reverse
    }

    def take(n: Int): Stream[A] = this match {
        case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n - 1))
        case Cons(h, _) if n == 1 => Stream.cons(h(), Stream.empty)
        case _ => Stream.empty
    }

    /*
    returning all starting elements of a Stream that
    match the given predicate.
     */
    def takeWhile(p: A => Boolean): Stream[A] = this match {
        case Empty => Empty
        case Cons(h, t) => {
            if (p(h())) Stream.cons(h(),  t().takeWhile(p))
            else Empty
        }
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
        case Empty => z
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
    }

    /*
     Evaluation will stop as soon as predicate(x) yields true in recursion
     */
    def exist2(predicate: A => Boolean): Boolean = {
        foldRight(false)((x, y) => {
            predicate(x) || y
        })
    }

    def exist(predicate: A => Boolean): Boolean = this match {
        case Empty => false
        case Cons(h, t) => predicate(h()) || t().exist(predicate)
    }
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
    /*
    The toto: => B type annotation.
     indicates that the argument is of type B, but won’t be
evaluated until it’s needed by the function
     */ def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
        lazy val head = hd
        lazy val tail = tl
        Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}


