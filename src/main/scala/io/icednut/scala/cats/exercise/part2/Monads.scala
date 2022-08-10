package io.icednut.scala.cats.exercise.part2

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Monads {

  // lists
  val numbersList = List(1, 2, 3)
  val charsList = List('a', 'b', 'c')

  // TODO 1.1: how do you create all combinations of (number, char)?
  val myResult1 = for {
    n <- numbersList
    c <- charsList
  } yield {
    (n, c)
  } // identical
  val combinationsList = numbersList.flatMap(n => charsList.map(c => (n, c)))

  // options
  val numberOption = Option(2)
  val charOption = Option('d')

  // TODO 1.2: how do you create the combination of (number, char)?
  val myResult2 = for {
    n <- numberOption
    c <- charOption
  } yield {
    (n, c)
  }

  // futures
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val numberFuture = Future(42)
  val charFuture = Future('Z')

  // TODO 1.3: how do you create the combination of (number, char)?
  val myResult3: Future[(Int, Char)] = for {
    nf <- numberFuture
    cf <- charFuture
  } yield {
    (nf, cf)
  }

  /**
   * Pattern
   * - wrapping a value into a monadic value
   * - the flatMap mechanism
   *
   * MONAD
   */

  trait MyMonad[M[_]] {
    def pure[A](value: A): M[A]

    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(x => pure[B](f(x)))
  }

  // Cats Monad
  import cats.Monad
  import cats.instances.option._ // implicit Monad[Option]

  val optionMonad = Monad[Option]
  val anOption = optionMonad.pure(4) // Option(4) == Some(4)
  val aTransformedOption = optionMonad.flatMap(anOption)(x => if (x % 3 == 0) Some(x + 1) else None)

  import cats.instances.list._
  val listMonad = Monad[List]
  val aList = listMonad.pure(3) // List(3)
  val aTransformedList = listMonad.flatMap(aList)(x => List(x, x + 1))

  // TODO 2: use a Monad[Future]
  import cats.instances.future._
  val futureMonad = Monad[Future]
  val aFuture = futureMonad.pure(42)
  val aTransformedFuture = futureMonad.flatMap(aFuture)(x => Future(x + 1))

  // specialized API
  def getPairsList(numbers: List[Int], chars: List[Char]): List[(Int, Char)] = numbers.flatMap(n => chars.map(c => (n, c)))
  def getPairsOption(numbers: Option[Int], chars: Option[Char]): Option[(Int, Char)] = numbers.flatMap(n => chars.map(c => (n, c)))
  def getPairsFuture(numbers: Future[Int], chars: Future[Char]): Future[(Int, Char)] = numbers.flatMap(n => chars.map(c => (n, c)))

  // generalize
  def getPairs[M[_], A, B](ma: M[A], mb: M[B])(implicit monad: Monad[M]): M[(A, B)] =
    monad.flatMap(ma)(a => monad.map(mb)(b => (a, b)))

  // extension method - weirder imports - pure, flatmap
  import cats.syntax.applicative._ // pure is here
  val oneOption = 1.pure[Option] // implicit Monad[Option] will be use => Some(1)
  val oneList = 1.pure[List]

  import cats.syntax.flatMap._ // flatMap is here
  val oneOptionTransformed = oneOption.flatMap(x => (x + 1).pure[Option])

  // TODO 3: implement the map method in MyMonad
  // Monads extend Functors
  val oneOptionMapped = Monad[Option].map(Option(2))(_ + 1)
  import cats.syntax.functor._ // map is here.
  val oneOptionMapped2 = oneOption.map(_ + 2)

  // for-comprehensions
  val composedOptionFor = for {
    one <- 1.pure[Option]
    two <- 2.pure[Option]
  } yield one + two

  // TODO 4: implement a shorter version of getPairs using for-comprehensions
  def getPairsWithFor[M[_]: Monad, A, B](ma: M[A], mb: M[B]): M[(A, B)] =
    for {
      a <- ma
      b <- mb
    } yield {
      (a, b)
    }

  def main(args: Array[String]): Unit = {
    println(myResult1)
    println(combinationsList)

    println(myResult2)

    myResult3.foreach(println)

    println(aTransformedOption)
    println(aTransformedList)
    aTransformedFuture.foreach(println)

    println(getPairs(numbersList, charsList))
    println(getPairs(numberOption, charOption))
    println(getPairsWithFor(numbersList, charsList))
    println(getPairsWithFor(numberOption, charOption))
    getPairs(numberFuture, charFuture).foreach(println)
  }
}
