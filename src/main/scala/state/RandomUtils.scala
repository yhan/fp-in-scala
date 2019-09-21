package state

import scala.annotation.tailrec

object RandomUtils {

    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
        @tailrec def walk(list: List[Int], rng: RNG, step: Int): (List[Int], RNG) = {
            if (step == count) {
                return (list, rng)
            }

            val (x, r) = rng.nextInt
            println(x)
            walk(x :: list, r, step + 1)
        }

        walk(List[Int](), rng, 0)
    }

    def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
        if (count == 0) {
            (List[Int](), rng)
        } else {
            val (x, r) = rng.nextInt
            val (list, r2) = ints2(count - 1)(r)

            (x :: list, r2)
        }
    }


    type Rand[+A] = RNG => (A, RNG)

    def unit[A](a: A): Rand[A] = rng => (a, rng)

    def nonNegativeInt(random: RNG): (Int, RNG) = {
        val (a, r) = random.nextInt
        ((if (a < 0) -a else a), r)
    }

    /*
    map for transforming the output of a state action without modifying the
    state itself
    */ def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
        val (a, rng2) = s(rng)
        (f(a), rng2)
    }

    def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

    /*
    Write a function to generate a Double between 0 and 1, not including 1. Note: You can
    use Int.MaxValue to obtain the maximum positive integer value, and you can use
    x.toDouble to convert an x: Int to a Double.
    def double(rng: RNG): (Double, RNG)
     */ def nextDouble(random: RNG): (Double, RNG) = {
        map(nonNegativeInt)(a => (if (a == Int.MaxValue) a - 1 else a) / Int.MaxValue.toDouble)(random)
    }

    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
        rng => {
            val (a, r) = ra(rng)
            val (b, rNext) = rb(r)
            val c = f(a, b)
            (c, rNext)
        }
    }

    def nextInt(rng: RNG): (Int, RNG) = {
        rng.nextInt
    }

    def intDouble(rng: RNG): ((Int, Double), RNG) = {
        map2(nextInt, nextDouble)((i, d) => (i, d))(rng)
    }

    def both[A, B](ra: Rand[A], rb: Rand[B]) : Rand[(A, B)] = {
        map2(ra, rb)((_, _))
    }

    def intDouble2(rng: RNG): ((Int, Double), RNG) = {
        both(nextInt, nextDouble)(rng)
    }

    def doubleInt(rng: RNG) : ((Double, Int), RNG) = {
        both(nextDouble,  nextInt)(rng)
    }

}