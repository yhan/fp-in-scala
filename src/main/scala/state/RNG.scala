package state

trait RNG {
    def nextInt: (Int, RNG)

    def double(): (Double, RNG)
}

object RNG {
    type Rand[+A] = RNG => (A, RNG)

    def unit[A](a: A): Rand[A] = rng => (a, rng)
}