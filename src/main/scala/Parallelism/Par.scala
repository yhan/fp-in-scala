package fpinscala.parallelism

import java.util.concurrent._

import scala.language.implicitConversions

object Par {
    /*
    A type alias
     */
    type Par[A] = ExecutorService => Future[A]

    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

    // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value.
    // It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.
    def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

    private case class UnitFuture[A](get: A) extends Future[A] {
        def isDone = true

        def get(timeout: Long, units: TimeUnit): A = get

        def isCancelled = false

        def cancel(evenIfRunning: Boolean): Boolean = false
    }

    def sortPar(parList: Par[List[Int]]): Par[List[Int]] = {
        map(parList)(l => l.sorted)
    }

    def sortPar2(parList: Par[List[Int]]): Par[List[Int]] = {
        val bidon: Par[Unit] = unit(())
        map2(parList, bidon)((a, _) => a.sorted)
    }

    def mapByMap2[A, B](a: Par[A])(f: A => B): Par[B] = {
        map2(a, unit(()))((x, _) => f(x))
    }

    def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = {
        ps.foldRight(unit(List[B]()))((a, acc) => map2(unit(a), acc)(f(_) :: _))
    }

    /*
      *  Combine N parallel computations, N equals number of elements in the list
      */ def parMap2[A, B](originList: List[A])(transform: A => B): Par[List[B]] = {
        val t: List[Par[B]] = originList.map(asyncFunction(transform))
        sequence(t)
    }

    def sequence[A](futures: List[Par[A]]): Par[List[A]] = {
        futures.foldRight(unit(List[A]()))((p, acc) => map2(p, acc)(_ :: _))
    }

    def sequenceFoldLeft[A](futures: List[Par[A]]): Par[List[A]] = {
        val start = unit(List[A]())
        futures.foldLeft(start)((acc, p) => map2(p, acc)(_ :: _))
    }

    /*
     *  transform a sync function to async one
     */ def asyncFunction[A, B](f: A => B): A => Par[B] = {
        //        a => fork(unit(f(a))) // delay exec here}
        a => lazyUnit(f(a))
    }

    // 'a' wrapped in a Parallel promise 'Par[A]', we apply a function on `a` which is supposed to yield a `B` instance,
    // we then obtain a Parallel promise of B instance
    def map[A, B](a: Par[A])(f: A => B): Par[B] = { es: ExecutorService => {
        val futureA: Future[A] = a(es)
        val value: A = futureA.get
        UnitFuture(f(value))
    }
    }


    def map3_2[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] = (es: ExecutorService) => {
        val g: C => Par[D] = (c: C) => map2[A, B, D](a, b)((aa, bb) => f(aa, bb, c))

        val parParD: Par[Par[D]] = map(c)(c => g(c))
        val futureParD: Future[Par[D]] = Par.run(es)(parParD)
        val parD = futureParD.get()
        Par.run(es)(parD)
    }

    def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] = (es: ExecutorService) => {
        val futureA = a(es)
        val futureB = b(es)
        val futureC = c(es)

        UnitFuture(f(futureA.get(), futureB.get(), futureC.get()))
    }

    //
    //    def map4[A, B, C, D, E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(f: (A,B,C,D) => E) : Par[E] = (es: ExecutorService) => {
    //        val dToE = (dd: D) => map3(a, b, c)((x, y, z) => f(a, b, c, ))
    //    }
    // `map2` doesn't evaluate the call to `f` in a separate logical thread,
    // in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism.
    // We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    // ********
    // This implementation of `map2` does _not_ respect timeouts. It simply passes the `ExecutorService`
    // on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them,
    // and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation
    // that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
    //
    // Combine two parallel computations
    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
        val af: Future[A] = a(es)
        val bf: Future[B] = b(es)
        UnitFuture(f(af.get, bf.get))
    }

    def paragraphWords(paragraph: String)(es: ExecutorService): Int = {
        val words = paragraph.split(' ').toList

        def count(wordsCollection: List[String])(es: ExecutorService): Int = {
            if (wordsCollection.length <= 1) {
                return wordsCollection.headOption.getOrElse("").length
            }
            val (l: List[String], r: List[String]) = wordsCollection.splitAt(wordsCollection.length)
            val tasks: Par[Int] = map2(unit(l), unit(r))((x, y) => {
                if (x.length == 1) return 1
                if (y.length == 1) return 1 else {
                    count(l)(es) + count(r)(es)
                }
            })

            Par.run(es)(fork(tasks)).get()
        }

        count(words)(es)
    }

    def countNumberOfWordsOfAllParagraphs(paragraphs: List[String])(es: ExecutorService): Int = {
        if (paragraphs.length <= 1) {
            return paragraphs.headOption.getOrElse("").length
        }
        val (l, r) = paragraphs.splitAt(paragraphs.length)
        val parWordsCount: Par[Int] = map2(unit(l), unit(r))((a, b) => {
            if (a.length == 1) paragraphWords(a.head)(es)
            if (b.length == 1) paragraphWords(b.head)(es)
            countNumberOfWordsOfAllParagraphs(a)(es) + countNumberOfWordsOfAllParagraphs(b)(es)
        })

        Par.run(es)(fork(parWordsCount)).get()
    }

    def maxValueOf(list: IndexedSeq[Int], es: ExecutorService): Int = {
        if (list.length <= 1) list.headOption.getOrElse(0) else {
            val (l, r) = list.splitAt(list.length / 2)
            val parMax: Par[Int] = map2(unit(l), unit(r))((a: IndexedSeq[Int], b) => {
                val lMax = maxValueOf(a, es)
                val rMax: Int = maxValueOf(b, es)

                if (lMax >= rMax) lMax else rMax
            })

            Par.run(es)(fork(parMax)).get()
        }
    }

    def sum(ints: IndexedSeq[Int], es: ExecutorService): Int = {
        if (ints.length <= 1) {
            ints.headOption.getOrElse(0)
        } else {
            val (l: IndexedSeq[Int], r: IndexedSeq[Int]) = ints.splitAt(ints.length / 2)
            val value: Par[Int] = map2(unit(l), unit(r))((x, y) => sum(x, es) + sum(y, es))

            val value1: Future[Int] = Par.run(es)(fork(value))
            value1.get()
        }
    }

    // `fork` should not affect the result of a parallel computation:
    // fork(x) == x
    // fork(x) should do the same thing as x, but asynchronously, in a logical thread separate from the main thread
    // --------------------------------------------------------------------------------------------------------------------
    // This is the simplest and most natural implementation of `fork`,
    // but there are some problems with it
    // -- for one, the outer `Callable` will block waiting for the "inner" task to complete.
    // Since this blocking occupies a thread in our thread pool,
    // or whatever resource backs the `ExecutorService`,
    // this implies that we're losing out on some potential parallelism.
    //
    // Essentially, we're using two threads when one should suffice.
    // This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    def fork[A](a: => Par[A]): Par[A] = { es =>
        es.submit(new Callable[A] {
            def call = a(es).get
        })
    }

    def delay[A](fa: => Par[A]): Par[A] =
        es => fa(es)

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def asyncF[A,B](f: A => B): A => Par[B] = a => {
        lazyUnit(f(a))
    }

}

object Examples {
    // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library.
    // Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    def sum(ints: IndexedSeq[Int]): Int = {
        if (ints.size <= 1) {
            ints.headOption getOrElse 0
        } // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3. else { else {
        val (l, r) = ints.splitAt(ints.length / 2) // Divide the sequence in half using the `splitAt` function.
        sum(l) + sum(r) // Recursively sum both halves and add the results together.}
    }
}
