package state

import scala.annotation.tailrec
import scala.collection.immutable

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


    def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

    /*
    Write a function to generate a Double between 0 and 1, not including 1. Note: You can
    use Int.MaxValue to obtain the maximum positive integer value, and you can use
    x.toDouble to convert an x: Int to a Double.
    def double(rng: RNG): (Double, RNG)
     */ def nextDouble(random: RNG): (Double, RNG) = {
        map(nonNegativeInt)(a => (if (a == Int.MaxValue) a - 1 else a) / Int.MaxValue.toDouble)(random)
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

    /*Implement sequence for combining a List of transitions into a single
    transition.

    !!!!!!!! BUG !!!!!!! why this function does not generate changing RNG (rNext)

    */
    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
            rng => {
                fs.foldRight((List[A](), rng))((rand, state) => {
                     val (next, rNext)  = rand(state._2)
                     (next :: state._1, rNext)
                })
            }
    }

    def ints3(count: Int)(rng: RNG): (List[Int], RNG) = {
        val listOfFactory: List[Rand[Int]] = List.fill[Rand[Int]](count)(nextInt)
        sequence(listOfFactory)(rng)
    }


    // In `sequence`, the base case of the fold is a `unit` action that returns
    // the empty list. At each step in the fold, we accumulate in `acc`
    // and `f` is the current element in the list.
    // `map2(f, acc)(_ :: _)` results in a value of type `Rand[List[A]]`
    // We map over that to prepend (cons) the element onto the accumulated list.
    //
    // We are using `foldRight`. If we used `foldLeft` then the values in the
    // resulting list would appear in reverse order. It would be arguably better
    // to use `foldLeft` followed by `reverse`. What do you think?
    def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] =
        fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))


    def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
       rng => {
           val (a, rng2) = f(rng)
           g(a)(rng2)
       }
    }

    def map2_2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {

          flatMap(ra)(a => map(rb)(b => f(a, b)))
    }

    /*
    map for transforming the output of a state action without modifying the
    state itself
    */def map[A, B](f: Rand[A])(g: A => B): Rand[B] = {
        rng => {
            val (a, rng2) = f(rng)
            (g(a), rng2)
        }
    }

    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
        rng => {
            val (a, r) = ra(rng)
            val (b, rNext) = rb(r)
            val c = f(a, b)
            (c, rNext)
        }
    }

    def map_2[A, B](f: Rand[A])(g: A => B): Rand[B] = {
        flatMap(f)( a=> unit(g(a)))
    }

    def nonNegativeLessThan2 (n: Int): Rand[Int] = {
        flatMap(nonNegativeInt)(i => {
            val mod = i % n
            if(i + (n-1) - mod >= 0) {
                unit(mod)
            }
            else {
                nonNegativeLessThan2(n)
            }
        })
    }

    def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
        val (i, rng2) = nonNegativeInt(rng)
        val mod = i % n
        if (i + (n-1) - mod >= 0) {
            (mod, rng2)
        }
        else {
             nonNegativeLessThan(n)(rng)
        }
    }

}