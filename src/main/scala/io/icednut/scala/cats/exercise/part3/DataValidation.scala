package io.icednut.scala.cats.exercise.part3

import cats.Semigroup

import scala.annotation.tailrec
import scala.util.Try

object DataValidation {

  import cats.data.Validated

  val aValidValue: Validated[String, Int] = Validated.valid(42) // "right" value
  val anInvalidValue: Validated[String, Int] = Validated.invalid("Something went wrong") // "left" value
  val aTest: Validated[String, Int] = Validated.cond(42 > 39, 99, "meaning of life is too small")

  def testPrime(n: Int): Boolean = {
    @tailrec
    def tailrecPrime(d: Int): Boolean =
      if (d <= 1) true
      else n % d != 0 && tailrecPrime(d - 1)

    if (n == 0 || n == 1 || n == -1) false
    else tailrecPrime(Math.abs(n / 2))
  }

  // TOOD: use Either
  /*
    - n must be a prime
    - n must be non-negative
    - n <= 100
    - n must be even
   */
  def testNumber(n: Int): Either[List[String], Int] = {
    // my answer
    //    var errorReason = Vector.empty[String]
    //
    //    if (n == 2) {
    //      errorReason = errorReason :+ "n is not prime"
    //    }
    //    if (n < 0) {
    //      errorReason = errorReason :+ "n is negative"
    //    }
    //    if (n > 100) {
    //      errorReason = errorReason :+ "n must under 100"
    //    }
    //    if (n % 2 != 0) {
    //      errorReason = errorReason :+ "n must be even"
    //    }
    //
    //    if (errorReason.isEmpty) {
    //      Right(n)
    //    } else {
    //      Left(errorReason.toList)
    //    }

    val isNotEven: List[String] = if (n % 2 == 0) List() else List("Number is not even")
    val isNegative: List[String] = if (n >= 0) List() else List("Number must be non-negative")
    val isToBig: List[String] = if (n <= 100) List() else List("Number must be less than or equal to 100")
    val isNotPrime: List[String] = if (testPrime(n)) List() else List("Number must be a prime")

    if (n % 2 == 0 && n >= 0 && n <= 100 && testPrime(n)) Right(n)
    else Left(isNotEven ++ isNegative ++ isToBig ++ isNotPrime)
  }

  import cats.instances.list._

  implicit val combineIntMax: Semigroup[Int] = Semigroup.instance[Int](Math.max)

  def validateNumber(n: Int): Validated[List[String], Int] =
    Validated.cond(n % 2 == 0, n, List("Number must be even"))
      .combine(Validated.cond(n >= 0, n, List("Number must be non-negative")))
      .combine(Validated.cond(n <= 100, n, List("Number must be <= 100")))
      .combine(Validated.cond(testPrime(n), n, List("Number must be a prime")))

  // chain
  aValidValue.andThen(_ => anInvalidValue)
  // test a valid
  aValidValue.ensure(List("something went wrong"))(_ % 2 == 0)
  // transform
  aValidValue.map(_ + 1)
  aValidValue.leftMap(_.length)
  aValidValue.bimap(_.length, _ + 1)
  // interperative with stdlib
  val eitherToValidated: Validated[List[String], Int] = Validated.fromEither(Right(42))
  val optionToValidated: Validated[List[String], Int] = Validated.fromOption(None, List("Nothing present here"))
  val tryToValidated: Validated[Throwable, Int] = Validated.fromTry(Try("somehting".toInt))
  // backwards
  aValidValue.toOption
  aValidValue.toEither

  // TODO 2: form validation
  object FormValidation {
    import cats.instances.string._
    type FormValidation[T] = Validated[List[String], T]

    def getValue(form: Map[String, String], fieldName: String): FormValidation[String] =
      Validated.fromOption(form.get(fieldName), List(s"The field $fieldName must be specified."))
    def nonBlank(value: String, fieldName: String): FormValidation[String] =
      Validated.cond(value.length >= 0, value, List(s"The field $fieldName must not be blank."))
    def emailProperForm(email: String): FormValidation[String] =
      Validated.cond(email.contains("@"), email, List("email is invalid."))
    def passwordCheck(password: String): FormValidation[String] =
      Validated.cond(password.length >= 10, password, List("password must be at least 10 characters long."))

    /*
      fileds are
      - name
      - email
      - password

      rules are
      - name, email and password MUST be specified
      - name must not be blank
      - email must have "@"
      - password must have >= 10 characters
     */
//    def validatedForm(form: Map[String, String]): FormValidation[String] =
//      Validated.fromOption(form.get("name").filter(_ != ""), List("name must not be blank"))
//        .combine(Validated.fromOption(form.get("email").filter(_.contains("@")), List("email must have '@'")))
//        .combine(Validated.fromOption(form.get("password").filter(_.length >= 10), List("password must have >= 10 characters")))
    def validatedForm(form: Map[String, String]): FormValidation[String] =
      getValue(form, "name").andThen(name => nonBlank(name, "name"))
        .combine(getValue(form, "email")).andThen(emailProperForm)
        .combine(getValue(form, "password")).andThen(passwordCheck)
        .map(_ => "Success")
  }

  import cats.syntax.validated._
  val aValidMeaningOfList = 42.valid[List[String]]
  val anError = "Something".invalid[Int]

  def main(args: Array[String]): Unit = {
    println(testNumber(400))
    println(validateNumber(400))

    println(FormValidation.validatedForm(Map()))
    println(FormValidation.validatedForm(Map("name" -> "", "email" -> "adsflk@asdlfdas", "password" -> "123")))
    println(FormValidation.validatedForm(Map("name" -> "asefdasf", "email" -> "adsflk@asdlfdas", "password" -> "123")))
    println(aValidMeaningOfList)
    println(anError)
  }
}
