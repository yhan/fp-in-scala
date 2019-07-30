package collections

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
    def depth[A](tree: Tree[A]): Int = tree match {
        case Leaf(_) => 1
        case Branch(left, right) => (1 + depth(left)) max (1 + depth(right))
    }

    def maximum(t: Tree[Int]): Int = t match {
        case Leaf(n) => n
        case Branch(l, r) => maximum(l) max maximum(r)
    }

    def size[A](tree: Tree[A]): Int = tree match {
        case Leaf(_) => 1
        case Branch(left, right) => 1 + size(left) + size(right)
    }

}



