package languageFeatures

object ImplicitTests extends App {
    val s: String = "Mark"

    def sayHello ={
        println("Hello")
    }

    case class HiString(name: String){
        def HiString() ={
            println("Hi " +  name)
        }
    }

    // Extend the java.lang.String, giving it the capacity to do HiString
    implicit def stringExt (n: String): HiString = HiString(n)

    s.HiString
}
