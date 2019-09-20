package state

trait RNG {
    def nextInt: (Int, RNG)
    def double(): (Double, RNG)
}
