package collections

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {
    def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))

    /**
     * time: O(m+n)
     * */
    def hasSubsequence[T](list1: List[T], list2: List[T]): Boolean = {

        def loop(a: List[T], b: List[T]): Boolean = (a) match {
            case _ if(startsWith(a, b))=> true
            case Nil =>  b == Nil
            case Cons(_, t) => hasSubsequence(t, b)
        }

        loop(list1, list2)
    }
    @annotation.tailrec
    def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
        case (_,Nil) => true
        case (Cons(h,t),Cons(h2,t2)) if h == h2 => {
            startsWith(t, t2)
        }
        case _ => false
    }

    @annotation.tailrec
    def hasSubsequence2[A](sup: List[A], sub: List[A]): Boolean = {
        sup match {
            case Nil => sub == Nil
            case x if startsWith(sup, sub) => {
                true
            } // if startWith returns false, will jump into next case matching
            case Cons(_, t) => {
                hasSubsequence2(t, sub)
            }
        }
    }

    def zipWith[A, B, C](list1: List[A], list2: List[B])(function: (A, B) => C): List[C] = (list1, list2) match {
        case (Nil, _) => Nil
        case (_, Nil) => Nil
        case (Cons(x, xs), Cons(y, ys)) => Cons(function(x, y), zipWith(xs, ys)(function))
    }

    def addPairwise(list1: List[Int], list2: List[Int]): List[Int] = (list1, list2) match {
        case (Nil, _) => Nil
        case (_, Nil) => Nil
        case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addPairwise(xs, ys))
    }

    /**
     * Represent an iteration structure
     **/
    @scala.annotation.tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
        case Nil => z
        case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

    /**
     * Represent an recursion structure
     **/
    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    def map[A, B](list: List[A])(convert: A => B): List[B] = list match {
        case Nil => Nil
        case Cons(x, xs) => Cons(convert(x), map(xs)(convert))
    }

    def map2[A, B](list: List[A])(convert: A => B): List[B] = {
        foldRight(list, Nil: List[B])((x, ys) => Cons(convert(x), ys))
    }

    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = {
        foldRight(list, Nil: List[B])((a, listOfB) => appendByFold(f(a), listOfB))
    }

    def filterUsingFlatMap[T](list: List[T])(f: T => Boolean): List[T] = {
        flatMap(list)(t => {
            if (f(t)) List[T](t) else Nil: List[T]
        })
    }

    def map3[A, B](list: List[A])(convert: A => B): List[B] = {
        foldLeft(list, Nil: List[B])((xs, x) => Cons(convert(x), xs)) // <== will yield a reversed list
    }

    def filter[A](list: List[A])(f: A => Boolean): List[A] = {
        foldRight(list, Nil: List[A])((a, b) => {
            if (f(a)) Cons(a, b) else b
        })
    }

    def appendByFold[A](left: List[A], right: List[A]): List[A] = {
        foldRight(left, right)(Cons(_, _))
    }

    def flattenByRightFold[A](listOfLists: List[List[A]]): List[A] = {
        foldRight(listOfLists, Nil: List[A])(appendByFold)
    }

    def flattenByLeftFold[A](listOfLists: List[List[A]]): List[A] = {
        foldLeft(listOfLists, Nil: List[A])(appendByFold)
    }

    // length of a1=M, a2=N
    // complexity O(M)
    //  @tailrec  => Is not tail recursion
    def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
        case Nil => a2
        case Cons(h, t) => Cons(h, append(t, a2))
    }


    def reverse[A](list: List[A]): List[A] = {
        foldLeft(list, Nil: List[A])((x, y) => Cons(y, x))
    }

    def productLeftFold(list: List[Int]): Int = {
        foldLeft(list, 1)(_ * _)
    }


    // Build a new list from 'source', using all its elements excluding the last one.
    // length of source M: complexity = O(M^2)
    def init[A](source: List[A]): List[A] = {
        val newOne = List[A]()

        @scala.annotation.tailrec def loop(src: List[A], buffer: List[A]): List[A] = src match {
            case Nil => Nil
            case Cons(head, tail) => if (tail == Nil) {
                return buffer
            } else loop(tail, append(buffer, List[A] {
                head
            }))
        }

        loop(source, newOne)
    }

    def initRecursiveUsingFrames[A](l: List[A]): List[A] = l match {
        case Nil => sys.error("init of empty list")
        case Cons(_, Nil) => Nil
        case Cons(h, t) => Cons(h, init(t))
    }

    def sum(ints: List[Int]): Int = ints match {
        case Nil => 0
        case Cons(x, xs) => x + sum(xs)
    }


    def sum2(ints: List[Int]): Int = {
        foldRight(ints, 0)(_ + _)
    }

    def sumLeftFold(list: List[Int]): Int = {
        return foldLeft(list, 0)(_ + _)
    }


    def lengthLeftFold[A](list: List[A]): Int = {
        foldLeft(list, 0)((y, _) => y + 1)
    }


    def length[A](list: List[A]): Int = {
        foldRight(list, 0)((_, xs) => 1 + xs)
    }

    def selfConstructor(list: List[Int]): List[Int] = {
        //The type annotation Nil:List[Int] is needed here, because otherwise Scala infers the B type parameter in
        //foldRight as List[Nothing].
        foldRight(list, Nil: List[Int])(Cons(_, _))
    }

    /**
     * EXERCISE 3.7
     * Can product, implemented using foldRight, immediately halt the recursion and
     * return 0.0 if it encounters a 0.0? Why or why not? Consider how any short-circuiting
     * might work if you call foldRight with a large list. This is a deeper question that we’ll
     * return to in chapter 5.*/
    def foldRightWithCircuitBreaker[A, B](as: List[A], z: B, broken: B)(f: (A, B) => B)(circuit: A => Boolean): B = as match {
        case Nil => z
        case Cons(x, xs) => {
            if (circuit(x)) return broken else {
                f(x, foldRight(xs, z)(f))
            }
        }
    }

    def product2(ds: List[Double]): Double = {
        foldRightWithCircuitBreaker(ds, 1.0, 0.0)((x, y) => x * y)(d => d == 0)
    }

    def product(ds: List[Double]): Double = ds match {
        case Nil => 1.0
        case Cons(0.0, _) => 0.0
        case Cons(x, xs) => x * product(xs)
    }


    //  Implement the function tail for removing the first element of a List. Note that the
    //  function takes constant time. What are different choices you could make in your
    //  implementation if the List is Nil? We’ll return to this question in the next chapter
    def tail2[A](as: List[A]): List[A] = as match {
        case Cons(_, xs) => xs
        case Nil => Nil
    }

    /** Change the head of list    */
    def setHead[A](head: A, list: List[A]): List[A] = list match {
        case Cons(_, xs) => Cons(head, xs)
        case Nil => Cons(head, Nil)
    }

    //    /**
    //     * Drop the the first N elements*/
    //    def drop[A](l: List[A], removeCount: Int): List[A] = {
    //        @scala.annotation.tailrec
    //        def inner[A](temp: List[A], count: Int): List[A] = temp match {
    //            case Cons(x, xs) => {
    //                if (count < removeCount) inner(xs, count + 1) else temp
    //            }
    //            case Nil => Nil
    //        }
    //
    //        inner(l, 0)
    //    }
    def drop[A](l: List[A], removeCount: Int): List[A] = l match {
        case Cons(x, xs) => if (removeCount > 0) drop[A](xs, removeCount - 1) else l
        case Nil => Nil
    }


    @scala.annotation.tailrec def drop2[A](l: List[A], removeCount: Int): List[A] = l match {
        case Cons(x, xs) => {
            if (removeCount > 0) drop2(xs, removeCount - 1) else l
        }
        case _ => l
    }


    def drop3[A](l: List[A], n: Int): List[A] = if (n <= 0) l else l match {
        case Nil => Nil
        case Cons(_, t) => drop(t, n - 1)
    }

    /**
     * Curried
     * ============================
     * a function of two arguments can be represented as a function that
     * accepts one argument and returns another function of one argument.
     **/
    @scala.annotation.tailrec def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
        case Cons(x, xs) => {
            if (f(x)) dropWhile(xs)(f) else l
        }
        case Nil => Nil
    }


}

