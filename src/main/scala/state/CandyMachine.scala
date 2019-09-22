package state
import scala.collection.immutable

sealed trait Input
case object Coin extends Input
case object Turn extends Input

// Page 90 exercise 6.11

case class Machine(locked: Boolean, candies: Int, coins: Int){
    def simulateMachine2(inputs: List[Input]): State[Machine, (Int, Int)] = {
        val last = inputs.foldRight(this)((input, s) => {
            updat(input, s)
        })

        State.unit(last.candies, last.coins)
    }

    def updat(i: Input, s: Machine): Machine = {
        (i, s) match {
            case (_, Machine(_ /*locked*/ , 0 /*candies*/ , _ /*coins*/)) => s
            case (Coin, Machine(false, _, _)) => {
                s
            }
            case (Turn, Machine(true, _, _)) => s
            case (Coin, Machine(true, candy, coin)) => {
                Machine(locked = false, candies = candy, coins = coin + 1)
            }
            case (Turn, Machine(false, candy, coin)) => Machine(locked = true, candies = candy - 1, coins = coin)
        }
    }

}

object Candy {

    // je ne comprends rien!!!
    def simulateMachine2(inputs: List[Input]): State[Machine, (Int, Int)] = {

        val list: List[State[Machine, Unit]] = inputs.map(State.modify[Machine] _ compose (update))

        val seq: State[Machine, List[Unit]] = State.sequence(list)
            seq.flatMap(_ => State.get.map(s => (s.coins, s.candies)))
    }

    def update = (i: Input) => (s: Machine) =>
        (i, s) match {
            case (_, Machine(_, 0, _)) => s
            case (Coin, Machine(false, _, _)) => s
            case (Turn, Machine(true, _, _)) => s
            case (Coin, Machine(true, candy, coin)) =>
                Machine(false, candy, coin + 1)
            case (Turn, Machine(false, candy, coin)) =>
                Machine(true, candy - 1, coin)
        }
}

