package collections

sealed trait List2[+A]

case object Nil extends List2[Nothing]

case class Cons[+A](head: A, tail: List2[A]) extends List2[A]


object List2 {

  // Build a new list from 'source', using all its elements excluding the last one.
  // length of source M: complexity = O(M^2)
  def init[A](source: List2[A]): List2[A] = {
    val newOne = List2[A]()

    @scala.annotation.tailrec
    def loop(src: List2[A], buffer: List2[A]): List2[A] = src match {
      case Nil => Nil
      case Cons(head, tail) => if (tail == Nil) {
        return buffer
      } else loop(tail, append(buffer, List2[A] {  head  }))
    }

    loop(source, newOne)
  }


  def initRecursiveUsingFrames[A](l: List2[A]): List2[A] = l match {
    case Nil => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }


  // length of a1=M, a2=N
  // complexity O(M)
  def append[A](a1: List2[A], a2: List2[A]): List2[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  def sum(ints: List2[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def sum2(ints: List2[Int]): Int = {
    foldRight(ints, 0)((x, y) => x + y)
  }

  /**
   * foldRight just represent a recursion structure
   * */
  def foldRight[A, B](as: List2[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def selfConstructor(list : List2[Int]) : List2[Int] = {
    foldRight(list, Nil: List2[Int])(Cons(_, _))
  }

  /**
   * EXERCISE 3.7
   * Can product, implemented using foldRight, immediately halt the recursion and
   * return 0.0 if it encounters a 0.0? Why or why not? Consider how any short-circuiting
   * might work if you call foldRight with a large list. This is a deeper question that we’ll
   * return to in chapter 5.*/
  def foldRightWithCircuitBreaker[A, B](as: List2[A], z: B, broken: B)(f: (A, B) => B)(circuit: A => Boolean): B = as match {
    case Nil => z
    case Cons(x, xs) => {
      if(circuit(x)) return broken
      else {
        f(x, foldRight(xs, z)(f))
      }
    }
  }

  def product2(ds: List2[Double]): Double = {
    foldRightWithCircuitBreaker(ds, 1.0, 0.0)((x, y) => x * y)( d => d == 0)
  }

  def product(ds: List2[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List2[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))


  //  Implement the function tail for removing the first element of a List. Note that the
  //  function takes constant time. What are different choices you could make in your
  //  implementation if the List is Nil? We’ll return to this question in the next chapter
  def tail2[A](as: List2[A]): List2[A] = as match {
    case Cons(_, xs) => xs
    case Nil => Nil
  }

  /** Change the head of list    */
  def setHead[A](head: A, list: List2[A]): List2[A] = list match {
    case Cons(_, xs) => Cons(head, xs)
    case Nil => Cons(head, Nil)
  }

  /**
   * Drop the the first N elements*/
  def drop[A](l: List2[A], removeCount: Int): List2[A] = {
    @scala.annotation.tailrec def inner[A](temp: List2[A], count: Int): List2[A] = temp match {
      case Cons(x, xs) => {
        if (count < removeCount) inner(xs, count + 1) else temp
      }
      case Nil => Nil
    }

    inner(l, 0)
  }


  @scala.annotation.tailrec def drop2[A](l: List2[A], removeCount: Int): List2[A] = l match {
    case Cons(x, xs) => {
      if (removeCount > 0) drop2(xs, removeCount - 1) else l
    }
    case _ => l
  }


  def drop3[A](l: List2[A], n: Int): List2[A] = if (n <= 0) l else l match {
    case Nil => Nil
    case Cons(_, t) => drop(t, n - 1)
  }

  /**
   * Curried
   * ============================
   * a function of two arguments can be represented as a function that
   * accepts one argument and returns another function of one argument.
   **/
  @scala.annotation.tailrec def dropWhile[A](l: List2[A])(f: A => Boolean): List2[A] = l match {
    case Cons(x, xs) => {
      if (f(x)) dropWhile(xs)(f) else l
    }
    case Nil => Nil
  }
}