package languageFeatures

object ImplicitTests extends App {
    val s: String = "Mark"

    def sayHello ={
        println("Hello")
    }

    case class HiString(name: String){
        def sayHi() ={
            println("Hi " +  name)
        }
    }

    // Extend the java.lang.String, giving it the capacity to do sayHi
    implicit def stringExt (n: String): HiString = HiString(n)

    // Demo java.lang.String extended with capacity to do sayHi
    s.sayHi

    // Demo declare an implicit value which can be used 'implicitly' when used a function implicit parameter
    def repeatSayingHi(times: Int)(implicit hiString: HiString) = {
        for(i <- 0  to times){
            hiString.sayHi
        }
    }

    implicit val hiString : HiString = HiString("Steve")
    repeatSayingHi(10)
}
