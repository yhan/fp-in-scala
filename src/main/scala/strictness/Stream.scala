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
     */ def takeWhile(p: A => Boolean): Stream[A] = this match {
        case Empty => Empty
        case Cons(h, t) => {
            if (p(h())) Stream.cons(h(), t().takeWhile(p)) else Empty
        }
    }

    def takeWhile2(p: A => Boolean): Stream[A] = {
        foldRight(Stream.empty[A])((x, stream) => {
            if (p(x)) Stream.cons(x, stream) else Stream.empty[A]
        })
    }

    /*
     Evaluation will stop as soon as predicate(x) yields true in recursion
     */ def exist2(predicate: A => Boolean): Boolean = {
        foldRight(false)((x, y) => {
            predicate(x) || y
        })
    }

    /*
     Evaluation will stop as soon as predicate(x) yields true in recursion
     */ def exist(predicate: A => Boolean): Boolean = this match {
        case Empty => false
        case Cons(h, t) => predicate(h()) || t().exist(predicate)
    }

    def forAll(p: A => Boolean): Boolean = {
        foldRight(true)((x, y) => {
            println(x)
            p(x) && y
        })
    }

    def forAll2(p: A => Boolean): Boolean = this match {
        case Empty => true
        case Cons(h, t) => p(h()) && t().forAll(p)
    }

    def headOption(): Option[A] = this match {
        case Empty => None
        case Cons(h, t) => Some(h())
    }

    //Hard: Implement headOption using foldRight.
    def headOption2(): Option[A] = {
        foldRight(None: Option[A])((h: A, _) => Some(h))
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
        case Empty => z
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
    }

    def foldLeft[B](z: B)(f: (B, A) => B): B = this match {
        case Empty => z
        case Cons(h, t) => t().foldLeft(f(z, h()))(f)
    }

    def flatMapByRightFold[B](project: A => Stream[B]): Stream[B] = {
        foldRight(Stream.empty[B])((a, bStream) => project(a).append(bStream)) // <======= ** appending in reversed order comparing to flatMapByLeftFold **
    }

    def flatMapByLeftFold[B](project: A => Stream[B]): Stream[B] = {
        foldLeft(Stream.empty[B])((bStream, a) => bStream.append(project(a)))
    }

    /*The append method
should be non-strict in its argument.*/
    def append[B >: A](s: => Stream[B]): Stream[B] = {
        foldRight(s)((h, t) => Stream.cons(h, t))
    }

    def map[B](project: A => B): Stream[B] = {
        foldRight(Stream.empty[B])((a, b) => Stream.cons(project(a), b))
    }

    def filter(filter: A => Boolean): Stream[A] = {
        foldRight[Stream[A]](Stream.empty[A])((a, b) => {
            if (filter(a)) Stream.cons(a, b) else b
        })
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

    def constant[A](a: A): Stream[A] = {
        lazy val v: Stream[A] = cons(a, v)
        v
    }

    def constantByUnfolding[A](a: A): Stream[A] = {
        Stream.unfold(a)(s => Some(s, s))
    }

    // Write a more general stream-building function called unfold. It takes an initial state,
    // and a function for producing both the next state and the next value in the generated
    // stream.
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
        f(z) match {
            case Some((a, s)) => cons(a, unfold(s)(f))
            case None => empty[A]
        }
    }
}

object StreamExtensions {
    /*
    Produce infinite stream containing contiguous integers start from n
     */
    def from(n: Int): Stream[Int] = {
        Stream.cons(n, from(n + 1))
    }

    /*
    0,1,1,2,3,5,8
    */ val fibs = {
        def go(f0: Int, f1: Int): Stream[Int] = Stream.cons(f0, go(f1, f0 + f1))

        go(0, 1)
    }



    val fibByUnfolding2  ={
        Stream.unfold((0,1))(s => s match {
            case (f0, f1) => Some(f0, (f1, f0+f1))
        })
    }

    /*
    Produce infinite stream containing contiguous integers start from n
     */
    def fromByUnfolding(n: Int): Stream[Int] = {
        Stream.unfold(n)(s => Some((s, s+1)))
    }

    val ones = {
        Stream.unfold(1)(s => Some(s, s))
    }
}



