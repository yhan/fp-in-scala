package state

import state.RNG.Rand
import state.State._
import sun.security.util.Length

import scala.collection.immutable

/*
A `statement`
 */
case class State[S, +A](run: S => (A, S)) {

    def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
        val (a, s1) = run(s)
        val stateB: State[S, B] = f(a)
        stateB.run(s1)
    })

    def map[B](f: A => B): State[S, B] = State(s => {
        val (a, s1) = run(s)
        unit(f(a)).run(s1)
    })

    def _map[B](f: A => B): State[S, B] = {
        flatMap(a => unit(f(a)))
    }

    def map2[B, C](stateB: State[S, B])(f: (A, B) => C): State[S, C] = {
        flatMap(a => stateB.map(b => f(a, b)))
    }

}

object State {
    type Rand[A] = State[RNG, A]

    def unit[S, A](a: A): State[S, A] = State(s => (a, s))

    def sequence[S, A](l: List[State[S, A]]): State[S, List[A]] = {
        val empty = List[A]()
        l.foldRight(unit[S, List[A]](empty))((stateOfA, acc) => stateOfA.map2(acc)(_ :: _))
    }

    def modify[S](f: S => S): State[S, Unit] = for {
        s <- get // Gets the current state and assigns it to `s`.
        _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
    } yield ()

    def get[S]: State[S, S] = State(s => (s, s))

    def set[S](s: S): State[S, Unit] = State(_ => ((), s))

    def ns(int: State.Rand[Int]): Rand[List[Int]] =
        int.flatMap(x =>
            int.flatMap(y =>
                ints(x).map((xs: List[Int]) =>
                    xs.map(_ % y))))


    def ns2(int: State.Rand[Int]): Rand[List[Int]] =
        int.flatMap(x =>
                ints(x).map((xs: List[Int]) =>
                    xs.map(_ % x)))

    def nsSugar(int: State.Rand[Int]): Rand[List[Int]] = for {
        x <- int
        xs  <- ints(x)
    } yield xs.map(h => h % x)



    def ints(length: Int): Rand[List[Int]] ={
        val rng = SimpleRNG(42)
        val l: (List[Int], RNG) = RandomUtils.ints(length)(rng)

        unit(l._1)
    }
}