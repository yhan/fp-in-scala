package state

import org.scalatest.FunSuite

class CandyMachineShould extends FunSuite{
    test("Inserting a coin into a locked machine will cause it to unlock if there’s any candy left.") {
        val machine = Machine(locked = true, candies = 1, coins = 0)
        val machineState: State[Machine, (Int, Int)] = machine.simulateMachine2(List[Input](Coin))

        val last =  machineState.run(machine)

        assertResult(false)(last._2.locked) // TODO FAILING TEST
    }

    test("Cheated test") {
        val machine = Machine(locked = true, candies = 1, coins = 0)

        val simulation = Candy.simulateMachine2(List[Input](Coin))
        val last =  simulation.run(machine)

        assertResult((false))(last._2.locked)
    }

    test("d00")     {

        val state = State.unit[Machine, (Int, Int)](1, 42)
        val last = state.run(Machine(true, 2, 43))
        println(last)
    }

    test("Compose function") {
        val f = (i: Int ) => {
            println("negate " +  i)
            -i
        }
        val f2 = (i:Int) => {
            println("double " +  i)
            2 * i
        }
        val composed = f compose f2
        assertResult(-42*2)(composed(42))
    }

    test("Chain function") {
        val f = (i: Int ) => {
            println("negate " +  i)
            -i
        }
        val f2 = (i:Int) => {
            println("double " +  i)
            2 * i
        }
        val composed = f andThen  f2
        assertResult(-42*2)(composed(42))
    }
}
