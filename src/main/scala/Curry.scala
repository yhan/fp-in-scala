trait Curry {

  //  Let’s look at another example, currying,9 which converts a function f of two arguments
  //  into a function of one argument that partially applies f. Here again there’s only one
  //  implementation that compiles. Write this implementation.
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = { A => B => f(A, B)
  }

  //  Implement uncurry, which reverses the transformation of curry. Note that since =>
  //  associates to the right, A => (B => C) can be written as A => B => C.
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = { (A, B) => f(A)(B)
  }
}
