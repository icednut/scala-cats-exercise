package io.icednut.scala.cats.exercise.part4

import cats.Monad

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Semigroupals {

  trait MySemigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  import cats.Semigroupal
  import cats.instances.option._ // implicit Semigroupal[Option]
  val optionSemigroupal = Semigroupal[Option]
  val aTupledOption = optionSemigroupal.product(Some(123), Some("a string")) // Some((123, "a string"))
  val aNonTupled = optionSemigroupal.product(Some(123), None) // None

  import cats.instances.future._ // implicit Semigroupal[Future]
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val aTupledFuture = Semigroupal[Future].product(Future("THE MEaning of life"), Future(42)) // Future("...", 42)

  import cats.instances.list._
  val aTupleList = Semigroupal[List].product(List(1, 2), List("a", "b"))

  // TODOk: implement product with monads
  import cats.syntax.functor._ // for Map
  import cats.syntax.flatMap._ // for flatMap
  def productWithMonad[F[_], A, B](fa: F[A], fb: F[B])(implicit monad: Monad[F]): F[(A, B)] = {
//    monad.flatMap(fa)((a: A) => monad.map(fb)((b: B) => (a, b)))
    for {
      a <- fa
      b <- fb
    } yield {
      (a, b)
    }
  }

//  trait MyMonad[M[_]] extends MySemigroupal[M] {
//    def pure[A](value: A): M[A]
//    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
//    def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(x => pure[B](f(x)))
//
//    def product[A, B](fa: M[A], fb: M[B]): M[(A, B)] =
//      flatMap(fa)(a => map(fb)(b => (a, b)))
//  }

  // MONADS EXTEND SEMIGROUPALS

  // example: Validated
  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  val validatedSemigroupal = Semigroupal[ErrorsOr] // requires the implicit Semigroup[List[_]]

  val invalidsCombination = validatedSemigroupal.product(
    Validated.invalid(List("Something wrong", "Something else wrong")),
    Validated.invalid(List("This can't be right"))
  )

  type EitherErrorsOr[T] = Either[List[String], T]
  import cats.instances.either._ // implicit Monad[Either]
  val eitherSemigroupal = Semigroupal[EitherErrorsOr]
  val eitherCombination = eitherSemigroupal.product(
    Left(List("Something wrong", "Something else wrong")),
    Left(List("This can't be right"))
  )

  // Associativity: m.flatMap(f).flatMap(g) == m.flatMap(x => f(x).flatMap(g))

  // TODO 2: define a Semigroupal[List] which does a zip
  val listSemigroupal: Semigroupal[List] = new Semigroupal[List] {
    override def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] =
      fa.zip(fb)
  }
  val aZippedList: List[(String, String)] = listSemigroupal.product(
    List("1", "2"),
    List("a", "b")
  )

  def main(args: Array[String]): Unit = {
    println(aTupleList) // List((1, "a"), (2, "a"), (1, "b"), (2, "b"))
    println(invalidsCombination)
    println(eitherCombination)
    println(aZippedList)
  }
}
