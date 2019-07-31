package collections

sealed trait Tree[+A]

//case object NilTree extends Tree[Nothing]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

    def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
        case Leaf(x) => Leaf(f(x))
        case Branch(v, l, r) => Branch(f(v), map(l)(f), map(r)(f))
    }

    def fold[A, B](t: Tree[A])(valueFunc: A => B)(branchFunc: (A, B, B) => B): B = t match {
        case Leaf(value) => valueFunc(value)
        case Branch(v, left, right) => branchFunc(v, fold(left)(valueFunc)(branchFunc), fold(right)(valueFunc)(branchFunc))
    }

    def mapByFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
        fold(tree)(v => Leaf(f(v)): Tree[B])(branchFunc = (v: A, l: Tree[B], r: Tree[B]) => Branch(f(v), l, r))
    }

    def depthByFold[A](tree: Branch[A]): Int = {
        fold(tree)(_ => 1)((_, l, r) => 1 + (l max r))
    }

    def maximumByFold(tree: Branch[Int]): Int = {
        fold(tree)(x => x)((v, x, y) => v max x max y)
    }

    def sizeByFold[A](tree: Tree[A]): Int = {
        fold(tree)(_ => 1)((v, x, y) => 1 + x + y)
    }

    def depth[A](tree: Tree[A]): Int = tree match {
        case Leaf(_) => 1
        case Branch(v, l, r) => 1 + (depth(l) max depth(r))
    }

    def maximum(t: Tree[Int]): Int = t match {
        case Leaf(n) => n
        case Branch(v, l, r) => v max maximum(l) max maximum(r)
    }

    def size[A](tree: Tree[A]): Int = tree match {
        case Leaf(_) => 1
        case Branch(v, left, right) => 1 + size(left) + size(right)
    }

    def printDepthFirst[A](tree: Tree[A]): Unit = tree match {
        case Leaf(value) => println(value)
        case Branch(v, left, right) => {
            printDepthFirst(left)
            printDepthFirst(right)

            println(v)
        }
    }
}



