import scala.annotation.tailrec

sealed trait List2[+A]

case object Nil extends List2[Nothing]

case class Cons[+A](head: A, tail: List2[A]) extends List2[A]


object List2 {
  def sum(ints: List2[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
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
    def inner[A](temp: List2[A], count: Int): List2[A] = temp match {
      case Cons(x, xs) => {
        if (count < removeCount) inner(xs, count + 1) else temp
      }
      case Nil => Nil
    }

    inner(l, 0)
  }


  def drop2[A](l: List2[A], removeCount: Int): List2[A] = l match {
    case Cons(x, xs) => {
      if (removeCount > 0) drop2(xs, removeCount - 1) else l
    }
    case _ => l
  }


  def drop3[A](l: List2[A], n: Int): List2[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_,t) => drop(t, n-1)
    }

  @tailrec def dropWhile[A](l: List2[A], f: A => Boolean): List2[A] = l match {
    case Cons(x, xs) => {
      if (f(x)) dropWhile(xs, f) else l
    }
    case Nil => Nil
  }
}
