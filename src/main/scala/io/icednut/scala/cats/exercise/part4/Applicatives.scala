package io.icednut.scala.cats.exercise.part4

import cats.Functor
import cats.implicits.toFunctorOps

object Applicatives {

  // Applicatives = Functors + the pure methods
  import cats.Applicative
  import cats.instances.list._
  val listApplicative = Applicative[List]
  val aList = listApplicative.pure(2) // List(2)

  import cats.instances.option._ // implicit Applicative[Option]
  val optionApplicative = Applicative[Option]
  val anOption = optionApplicative.pure(2) // Some(2)

  // pure extension method
  import cats.syntax.applicative._
  val aSwwetList = 2.pure[List] // List(2)
  val aSweetOption = 2.pure[Option] // Some(2)

  // Monads extends Applicatives
  // Applicatives extends Functors
  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  val aValidValue: ErrorsOr[Int] = Validated.valid(43) // pure
  val aModifiedValidated: ErrorsOr[Int] = aValidValue.map(_ + 1) // map
  val validatedApplicative = Applicative[ErrorsOr]

  // TODO: thought experiment
  // def ap[W[_], B, T](wf: W[B => T])(wa: W[B]): W[T] = ??? // this already implimented
  def productWithApplicatives[W[_], A, B](wa: W[A], wb: W[B])(implicit applicative: Applicative[W]): W[(A, B)] = {
//    applicative.pure((applicative.map(wa)(a => a), applicative.map(wb)(b => b)))

//    applicative.ap(((a: A) => {
//      applicative.ap(((b: B) => {
//        (a, b)
//      }).pure[W])(wb)œ
//    }).pure[W])(wa)

    val functionWrapper: W[B => (A, B)] = applicative.map(wa)(a => (b: B) => (a, b))
    applicative.ap(functionWrapper)(wb)
  }

  def main(args: Array[String]): Unit = {
  }
}
