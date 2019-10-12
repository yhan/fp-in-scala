import java.net.UnknownServiceException

import org.scalatest.FunSuite

import scala.concurrent.Future

class MiscTests extends FunSuite{
    test("Equivalent to sum"){
        println((1 to 5).reduceLeft(_ + _))
    }

    test("Currying"){
        val zscore2 =
            (mean: Int, sd: Int) =>
                (x: Int) =>
                    (x - mean) / sd

        println(zscore2(1, 10) (20) )
    }

    test("Invoke with _"){
        def zscore(mean: Int, sd: Int)(x: Int) = {

            println(x) //41
            println(mean) //7
            println(sd) // 4
            (x - mean) / sd
        }

        val normer: Int => Int =
//            zscore(7, 4) _
            zscore(7, 4)

        println(normer(41))
    }

    test("partial function") {
        val isEven: PartialFunction[Int, Boolean] = {
             case x if x % 2 == 0 => true
        }

        val isOdd: PartialFunction[Int, Boolean] = {
            case x if x % 2 != 0 => false
        }

        assertResult(Vector(true, true))((1 to 5) collect isEven)

        assertResult(Vector(false, true, false, true, false))((1 to 5) collect (isEven orElse isOdd))
        assertResult(Vector(false, true, false, true, false))((1 to 5) map (isEven orElse(isOdd)))
    }

    test ("partial function 2") {
        val isEven : PartialFunction[Int, Boolean] = {
            case x if x %2==0 => true
            case _ => false
        }
        assertResult(Vector(false, true))((1 to 2) collect isEven)
    }

    test("Apply arguments on function") {
        // assign an object representing the function to a variable
        val f = (x:Int) => x + 1

        assertResult(42)(f.apply(41))
    }

    test("Construct"){
        val thomas = Person("Thomas")
        assertResult("Thomas")(thomas.companyName)
    }

    def getUser(): User = {
        User(42)
    }

//    test("Future pattern") {
//        val productsFuture = Future {
//            getUser()
//        }.map( user =>
//            Database.save(user))()
//        .map { dbResponse =>
//            Products.get(dbResponse.user.id)
//        }
//
//    }

    case class User(id: Int)

    object Database{
        def save(user: User): DbResponse = {
            DbResponse(user)
        }

    }

    case class DbResponse(user: User)


    object Products{
        def get(id: Int): List[Product] = {
            List(Product(42))
        }
    }

    case class Product(id: Int){
    }

}

case class Company(companyName: String)

class Person(val personName: String) {}

object Person {
    def apply(name: String): Company = new Company(name)
}


