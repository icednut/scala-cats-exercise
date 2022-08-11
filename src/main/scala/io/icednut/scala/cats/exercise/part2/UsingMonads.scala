package io.icednut.scala.cats.exercise.part2

import scala.collection.Map
import scala.util.{Failure, Success, Try}


object UsingMonads {

  import cats.Monad
  import cats.instances.list._

  val monadList = Monad[List]
  val aSimpleList: List[Int] = monadList.pure(2)
  val anExtendedList: List[Int] = monadList.flatMap(aSimpleList)(x => List(x, x + 1))

  type LoadingOr[T] = Either[String, T]
  type ErrorOr[T] = Either[Throwable, T]

  import cats.instances.either._

  val loadingMonad: Monad[LoadingOr] = Monad[LoadingOr]
  val anEither: LoadingOr[Int] = loadingMonad.pure(42)
  val aChangedLoading = loadingMonad.flatMap(anEither)(x => if (x % 2 == 0) Right(x + 1) else Left("Loading meaning of life..."))

  case class OrderStatus(orderId: Long, status: String)

  def getOrderStatus(orderId: Long) =
    Right(OrderStatus(orderId, "Ready to ship"))

  def trackLocation(orderStatus: OrderStatus) =
    if (orderStatus.orderId > 1000) Left("Not available")
    else Right("Amsterdam, NL")

  val orderId = 457L
  loadingMonad.flatMap(getOrderStatus(orderId))(x => trackLocation(x))

  // use extension methods
  val orderLocationBetter: LoadingOr[String] = getOrderStatus(orderId).flatMap(x => trackLocation(x))
  val orderLocationFor: LoadingOr[String] = for {
    x <- getOrderStatus(orderId)
    location <- trackLocation(x)
  } yield location

  // TODO: the service layer API of a web app
  import cats.syntax.flatMap._
  import cats.syntax.functor._
  case class Connection(host: String, port: String)

  val config = Map(
    "host" -> "localhost",
    "port" -> "4040"
  )

  trait HttpService[M[_]] {
    def getConnection(cfg: Map[String, String]): M[Connection]

    def issueRequest(connection: Connection, payload: String): M[String]
  }

  def getResponse[M[_]](service: HttpService[M], payload: String)(implicit monad: Monad[M]): M[String] =
    for {
      conn <- service.getConnection(config)
      response <- service.issueRequest(conn, payload)
    } yield {
      response
    }
  // DO NOT CHANGE THE CODE

  /*
    Requirements
    - if the host and port are found in the configuration map, then we'll return a M containing a connection with those values
      otherwise the method will fail, according to the logic of the type M
    - the issueRequest method returns a M containing the string: "request (payload) has been accepted", if the payload is less than 20 characters
      otherwise the method will fail, according to the logic of the type M

    TODO: provide a real implementation of HttpService using Try, Option, Future, Either
   */
  val tryMonad = Monad[Try]
  val httpServiceForTry: HttpService[Try] = new HttpService[Try] {
    override def getConnection(cfg: Map[String, String]): Try[Connection] =
      (cfg.get("host"), cfg.get("port")) match {
        case (Some(host), Some(port)) => Success(Connection(host, port))
        case _ => Failure(new RuntimeException("Invalid connection config"))
      }

    override def issueRequest(connection: Connection, payload: String): Try[String] = payload match {
      case _@p if payload.length < 20 => Success(s"request ($payload) has been accepted")
      case _ => Failure(new RuntimeException("Invalid payload"))
    }
  }

  object OptionHttpService extends HttpService[Option] {
    override def getConnection(cfg: Map[String, String]): Option[Connection] =
      for {
        h <- cfg.get("host")
        p <- cfg.get("port")
      } yield {
        Connection(h, p)
      }

    override def issueRequest(connection: Connection, payload: String): Option[String] =
      if (payload.length >= 20) None
      else Some(s"Request ($payload) has been accepted")
  }

  // TODO: implement another HttpService with LoadingOr or ErrorOr
  object LoadingOrHttpService extends HttpService[LoadingOr] {
    override def getConnection(cfg: Map[String, String]): LoadingOr[Connection] =
      (for {
        h <- cfg.get("host")
        p <- cfg.get("port")
      } yield {
        Connection(h, p)
      }) match {
        case Some(c) => Right(c)
        case _ => Left("Connection is not found")
      }

    override def issueRequest(connection: Connection, payload: String): LoadingOr[String] = {
      if (payload.length >= 20) Left("invalid payload")
      else Right(s"Request ($payload) has been accepted")
    }
  }

  def main(args: Array[String]): Unit = {
    val request1: Try[String] = for {
      connection <- httpServiceForTry.getConnection(config)
      requestResult <- httpServiceForTry.issueRequest(connection, "Hello, Monad")
    } yield {
      requestResult
    }
    val request2: Try[String] = for {
      connection <- httpServiceForTry.getConnection(config)
      requestResult <- httpServiceForTry.issueRequest(connection, "alsdkjflasdkjflaskdjflaksdjflkjs")
    } yield {
      requestResult
    }
    val request3: Option[String] = for {
      connection <- OptionHttpService.getConnection(config)
      requestResult <- OptionHttpService.issueRequest(connection, "alsdkjflasdkjflaskdjflaksdjflkjs")
    } yield {
      requestResult
    }
    val request4: LoadingOr[String] = for {
      connection <- LoadingOrHttpService.getConnection(config)
      requestResult <- LoadingOrHttpService.issueRequest(connection, "Hello, Monad")
    } yield {
      requestResult
    }

    println(request1)
    println(request2)
    println(request3)
    println(request4)
    println(getResponse(OptionHttpService, "Hello, Option"))
    println(getResponse(LoadingOrHttpService, "Hello, LoadingOr"))
  }
}
