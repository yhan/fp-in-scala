package strictness

sealed trait Stream[+A] {

    def ZipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
        Stream.unfold((this, s2))(s => s match {
            case (Cons(h, t), Cons(h2, t2)) => {
                val current = (Some(h()), Some(h2()))
                val nextState = (t(), t2())
                Some((current, nextState))
            }
            case (Empty, Cons(h2, t2)) => {
                val current = (None, Some(h2()))
                val nextState = (Empty, t2())
                Some(current, nextState)
            }
            case (Cons(h, t), Empty) => {
                val current = (Some(h()), None)
                val nextState = (t(), Empty)
                Some(current, nextState)
            }
            case (Empty, Empty) => None
        })
    }

    def ZipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = {
        Stream.unfold((this, s2))(s => s match {
            case (Cons(h, t), Cons(h2, t2)) => {
                val current = f(h(), h2())
                val nextState = (t(), t2())
                Some((current, nextState))
            }
            case (Empty, _) => None
            case (_, Empty) => None
        })
    }


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

    def takeByUnfolding(n: Int): Stream[A] = {
        Stream.unfold((this, n))(s => s match {
            case (Empty, _) => None
            case (Cons(h, t), count) => {
                if (count > 0) Some((h(), (t(), count - 1))) else None
            }
        })
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

    def takeWhileByUnfolding(p: A => Boolean): Stream[A] = {
        Stream.unfold(this)(s => s match {
            case Empty => None
            case Cons(h, t) => {
                if (p(h())) Some(h(), t()) else None
            }
        })
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
should be non-strict in its argument.*/ def append[B >: A](s: => Stream[B]): Stream[B] = {
        foldRight(s)((h, t) => Stream.cons(h, t))
    }

    def map[B](project: A => B): Stream[B] = {
        foldRight(Stream.empty[B])((a, b) => Stream.cons(project(a), b))
    }

    def mapByUnfolding[B](project: A => B): Stream[B] = {
        Stream.unfold(this)(s => s match {
            case Empty => None
            case Cons(h, t) => Some((project(h()), t()))
        })
    }

    def filter(filter: A => Boolean): Stream[A] = {
        foldRight[Stream[A]](Stream.empty[A])((a, b) => {
            if (filter(a)) Stream.cons(a, b) else b
        })
    }

    def startWith[A](sub: Stream[A]): Boolean = (this, sub) match {
        case (Empty, y) => y == Empty
        case (_, Empty) => true
        case (Cons(h, t), Cons(h2, t2)) => {
            if (h() == h2()) t().startWith(t2()) else false
        }
    }

    def hasSubsequence2[A](sub: Stream[A]): Boolean = this match {

        case Cons(h, t) /*if(t() != Empty)*/=> {
            try{
                if (Cons(h, t).startWith(sub)) true
                else t().startWith(sub)
            }catch {
                case ex : Exception => {
                    println(h())
                    println(ex)

                    false
                }
            }
        }
        case Empty => false
    }

    def hasSubsequence[A](sub: Stream[A]): Boolean = (this, sub) match {
        case (Cons(h, t), Cons(h2, t2)) => {
            if (h() == h2())
                t().startWith(t2())
            else
                t().hasSubsequence(sub)
        }
        case (Empty, y) => {
            println("error")
            y == Empty
        }
        case (_, Empty) => true
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
     */ def from(n: Int): Stream[Int] = {
        Stream.cons(n, from(n + 1))
    }

    /*
    0,1,1,2,3,5,8
    */ val fibs = {
        def go(f0: Int, f1: Int): Stream[Int] = Stream.cons(f0, go(f1, f0 + f1))

        go(0, 1)
    }


    val fibByUnfolding2 = {
        Stream.unfold((0, 1))(s => s match {
            case (f0, f1) => Some(f0, (f1, f0 + f1))
        })
    }

    /*
    Produce infinite stream containing contiguous integers start from n
     */ def fromByUnfolding(n: Int): Stream[Int] = {
        Stream.unfold(n)(s => Some((s, s + 1)))
    }

    val ones = {
        Stream.unfold(1)(s => Some(s, s))
    }
}



