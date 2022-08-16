package io.icednut.scala.cats.exercise.part2

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object MonadTransformers {

  def sumAllOptions(values: List[Option[Int]]): Int = ???

  // option transformer

  import cats.data.OptionT // Option Transformer
  import cats.instances.list._ // fetch an implicit Monad[List]
  import cats.instances.future._

  val listOfNumberOptions: OptionT[List, Int] = OptionT(List(Option(1), Option(2)))
  val listOfCharOptions: OptionT[List, Char] = OptionT(List(Option('a'), Option('b'), Option.empty[Char]))
  val listOfTuples: OptionT[List, (Int, Char)] = for {
    char <- listOfCharOptions
    number <- listOfNumberOptions
  } yield {
    (number, char)
  }

  // either transformer

  import cats.data.EitherT

  val listOfEithers: EitherT[List, String, Int] = EitherT(List(Left("something wrong"), Right(43), Right(2)))
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val futureOfEither: EitherT[Future, String, Int] = EitherT.right(Future(45)) // wrap Future(Right(45))

  /*
    TODO exercise
    We have a multi-machine cluster for your business which will receive a traffic surge following a media appearance.
    We measure bandwidth in units.
    We want to allocate TWO of our servers to cope with the traffic spike.
    We know the current capacity for each server and we know we'll hold the traffic if the sum of bandwidths is > 250.
   */
  val bandwidths = Map(
    "server1.rockthejvm.com" -> 50,
    "server2.rockthejvm.com" -> 300,
    "server3.rockthejvm.com" -> 170
  )

  type AsyncResponse[T] = EitherT[Future, String, T] // wrapper over Future[Either[String, T]]

  def getBandwidth(server: String): AsyncResponse[Int] = bandwidths.get(server) match {
//    case None => EitherT[Future, String, Int](Future(Left(s"Server $server unreachable")))
    case None => EitherT.left(Future(s"Server $server unreachable"))
//    case Some(b) => EitherT[Future, String, Int](Future(Right(b)))
    case Some(b) => EitherT.right(Future(b))
  }

  // TODO 1
  // hint: call getBandwidth twice, and combine the results
  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] = for {
    result1 <- getBandwidth(s1)
    result2 <- getBandwidth(s2)
  } yield {
    result1 + result2 > 250
  }
  // Future[Either[String, Boolean]]

  // TODO 2
  // hint: call canWithstandSurge + transform
  def generateTrafficSpikeReport(s1: String, s2: String): AsyncResponse[String] =
//    canWithstandSurge(s1, s2)
//      .map {
//        case true => s"'$s1' and '$s2' are not enough bandwidth"
//        case false => "SUCCESS"
//      }
  canWithstandSurge(s1, s2).transform {
    case Left(reason) => Left(s"Servers '$s1' and '$s2' CANNOT cope with the incoming spike: $reason")
    case Right(false) => Left(s"Servers '$s1' and '$s2' CANNOT cope with the incoming spike: not enough total bandwidth")
    case Right(true) => Right(s"Servers '$s1' and '$s2' can cope with the incoming spike NO PROBLEM!")
  }
  // Future[Either[String, String]]

  def main(args: Array[String]): Unit = {
    println(listOfTuples.value)
    printFutureResult(futureOfEither.value)
    printFutureResult(canWithstandSurge(s1 = "server1.rockthejvm.com", s2 = "server2.rockthejvm.com").value)
    printFutureResult(generateTrafficSpikeReport(s1 = "server1.rockthejvm.com", s2 = "server2.rockthejvm.com").value)
    generateTrafficSpikeReport(s1 = "server1.rockthejvm.com", s2 = "server3.rockthejvm.com").value.foreach(println)
    generateTrafficSpikeReport(s1 = "server5.rockthejvm.com", s2 = "server3.rockthejvm.com").value.foreach(println)
  }

  private def printFutureResult[T](future: Future[T]): Unit = future.onComplete {
    case Success(Right(v)) => println(v)
    case Success(Left(errorMessage)) => println(s"invalid result. error: $errorMessage")
    case Failure(exception) => println(s"error. message: ${exception.getMessage}")
  }
}
