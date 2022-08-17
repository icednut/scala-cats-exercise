package io.icednut.scala.cats.exercise.part3

import cats.Id
import cats.data.Kleisli

object Readers {

  /*
    - configuration file => inital data structure
    - a DB layer
    - an HTTP layer
    - a business logic layer
   */
  case class Configuration(dbUsername: String, dbPassword: String, host: String, port: Int, nThreads: Int, emailReplyTo: String)

  case class DbConnection(username: String, password: String) {
    def getOrderStatus(orderId: Long): String = "dispatched" // select * from db table and return the status of the orderID

    def getLastOrderId(username: String): Long = 54643 // select max(orderId) from table where username = username
  }

  case class HttpService(host: String, port: Int) {
    def start(): Unit = println("server started") // this would start the actual server
  }

  // bootstrap
  val config = Configuration(dbUsername = "daniel", dbPassword = "rockthejvm", host = "localhost", port = 1234, nThreads = 8, emailReplyTo = "rockthejvm@example.com")
  // cats Reader

  import cats.data.Reader

  val dbReader: Reader[Configuration, DbConnection] = Reader(conf => DbConnection(conf.dbUsername, conf.dbPassword))
  val dbConn = dbReader.run(config)

  // Reader[I, O]
  val danielsOrderStatusReader: Reader[Configuration, String] = dbReader.map(dbcon => dbcon.getOrderStatus(55))
  val danielOrderStatus: String = danielsOrderStatusReader.run(config)

  def getLastOrderStatus(username: String): String = {
    val usersLastOrderIdReader: Reader[Configuration, String] = dbReader
      .map(dbcon => dbcon.getLastOrderId(username))
      .flatMap(lastOrderId => dbReader.map(_.getOrderStatus(lastOrderId)))

    //    val usersLastOrderStatus = dbReader.map(_.getOrderStatus())

    val usersOrderFor: Kleisli[Id, Configuration, String] = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
    } yield orderStatus

    //    usersLastOrderIdReader.run(config)
    usersOrderFor.run(config)
  }

  // TODO 1 - email a user
  case class EmailService(emailReplyTo: String) {
    def sendEmail(address: String, contents: String): String = s"From: $emailReplyTo; to $address >>> $contents"
  }

  def emailUser(username: String, userEmail: String) = {
    // fetch the status of their last order
    // email them with the Email service: "Your order has the status: (status)"

    // My Answer
    //    val lastOrderStatus = getLastOrderStatus(username)
    //    EmailService(config.emailReplyTo).sendEmail(userEmail, s"Your order has the status: (${lastOrderStatus})")

    // Answer
    val emailServiceReader: Reader[Configuration, EmailService] = Reader(conf => EmailService(conf.emailReplyTo))
    //    val emailReader: Reader[Configuration, String] = emailServiceReader.map(reader => reader.sendEmail(userEmail, s"Your order has the status: (${lastOrderStatus})"))
    val emailReader: Reader[Configuration, String] = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
      emailService <- emailServiceReader
    } yield emailService.sendEmail(userEmail, s"Your order has the status: (${orderStatus})")

    emailReader.run(config)
  }

  // TODO 2: what programming pattern do Readers remind you of?
  // Dependency injection!

  def main(args: Array[String]): Unit = {
    println(getLastOrderStatus("daniel"))
    println(emailUser("daniel", "will@example.com"))
  }
}
