package functionalDesign

trait Monoid[A] {
    /*
    Satisfies
    op(op(x,y), z) == op(x, op(y,z))
     */ def op(a1: A, a2: A): A

    /*
    Satisfies op(x, zero) == x and op(zero, x) == x
    */
    def zero: A
}

object MonoidSpike {
    val intAddition: Monoid[Int] = new Monoid[Int] {
        override def op(a1: Int, a2: Int): Int = a1 + a2

        override def zero: Int = 0
    }

    val intMultiplication: Monoid[Int] = new Monoid[Int] {
        override def op(a1: Int, a2: Int): Int = a1 * a2

        override def zero: Int = 1
    }

    val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
        override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

        override def zero: Boolean = false
    }

    val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
        override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

        override def zero: Boolean = true
    }

    // Notice that we have a choice in how we implement `op`.
    // We can compose the options in either order. Both of those implementations
    // satisfy the monoid laws, but they are not equivalent.
    // This is true in general--that is, every monoid has a _dual_ where the
    // `op` combines things in the opposite order. Monoids like `booleanOr` and
    // `intAddition` are equivalent to their duals because their `op` is commutative
    // as well as associative.
    def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
        def op(x: Option[A], y: Option[A]) = x orElse y
        val zero = None
    }


}
