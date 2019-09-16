//package errorsHandling
//
//import cats.data.ValidatedNec
//import cats.implicits._
//
//
////=====================================================================
//case class Person(name: Name, age: Age){
//
//}
//
//
//sealed class Name(val value: String)
//sealed class Age(val value: Int)
//
//object Person{
//    def mkName(name: String): Either[String, Name] = {
//        if (name == "" || name == null) Left("Name is empty.") else Right(new Name(name))
//    }
//
//    def mkAge(age: Int): Either[String, Age] = {
//        if (age < 0) Left("Age is out of range.") else Right(new Age(age))
//    }
//
//    def mkPerson(name: String, age: Int): Either[String, Person] = {
//        mkName(name).map2(mkAge(age))(Person(_, _))
//    }
//}
//
//object validatedPerson extends App {
//    private def validateName(name: String): ValidatedNec[String, Name] =
//        if (name == "" || name == null) "Name is empty.".invalidNec else Name(name).validNec
//
//    private def validateAge(age: Int): ValidatedNec[String, Age] =
//        if (age < 0) "Age is out of range.".invalidNec else Age(age).validNec
//
//
//    def validatePerson(name: String, age: Int): ValidatedNec[String, Person] = {
//        (validateName(name), validateAge(age)).mapN(Person)
//    }
//
//    println(validatePerson(name = "Joe", age = 21))
//    println(validatePerson(name = "", age = -42))
//}