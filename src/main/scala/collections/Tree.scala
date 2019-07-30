package collections

import scala.annotation.tailrec

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
    def fold[A, B](t: Tree[A])(leafFunc: A => B)(branchFunc: (B, B) => B): B = t match {
        case Leaf(value) => leafFunc(value)
        case Branch(left, right) => branchFunc(fold(left)(leafFunc)(branchFunc), fold(right)(leafFunc)(branchFunc))
    }

    def mapByFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
        fold(tree)(v => Leaf(f(v)): Tree[B])((l, r) => Branch(l, r))
    }

    def depthByFold[A](tree: Branch[A]): Int = {
        fold(tree)(_ => 1)((l, r) => 1 + (l max r))
    }

    def maximumByFold(tree: Branch[Int]): Int = {
        fold(tree)(x => x)((x, y) => x max y)
    }

    def sizeByFold[A](tree: Tree[A]): Int = {
        fold(tree)(_ => 1)((x, y) => 1 + x + y)
    }

    def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
        case Leaf(x) => Leaf(f(x))
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

    def depth[A](tree: Tree[A]): Int = tree match {
        case Leaf(_) => 1
        case Branch(l, r) => 1 + (depth(l) max depth(r))
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



