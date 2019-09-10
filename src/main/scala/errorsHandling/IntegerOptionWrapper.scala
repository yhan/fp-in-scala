package errorsHandling

class IntegerOptionWrapper(intValue: Int) {
    private val option = Option[Int](intValue)

    def absolute(): Option[Int] = {
        val function = option.lift(math.abs)
        function(option)
    }
}
