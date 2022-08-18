package io.icednut.scala.cats.exercise.console

import cats.Show
import cats.effect.IO
import cats.effect.std.Console
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.funsuite.AsyncFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.charset.Charset
import scala.language.postfixOps

class FunctionalHelloTest extends AsyncFunSuite with AsyncIOSpec with Matchers {

  test("console exercise with cats") {
    implicit val testConsole = new Console[IO]() {
      var targetNormalMessages: Vector[String] = Vector()
      var targetErrorMessages: Vector[String] = Vector()
      var targetInputMessages: Vector[String] = Vector()

      override def readLineWithCharset(charset: Charset): IO[String] = IO {
        targetInputMessages.mkString
      }

      override def print[A](a: A)(implicit S: Show[A]): IO[Unit] = IO(targetNormalMessages :+= a.toString)

      override def println[A](a: A)(implicit S: Show[A]): IO[Unit] = IO {
        targetNormalMessages = targetNormalMessages :+ a.toString :+ "\n"
      }

      override def error[A](a: A)(implicit S: Show[A]): IO[Unit] = IO(targetErrorMessages :+= a.toString)

      override def errorln[A](a: A)(implicit S: Show[A]): IO[Unit] = IO {
        targetErrorMessages = targetErrorMessages :+ a.toString :+ """\n"""
      }

      def feedLines(message: String): IO[Unit] = {
        targetInputMessages :+= message
        IO.unit
      }

      def output: IO[Vector[String]] = IO(targetNormalMessages)
      def errorOutput: IO[Vector[String]] = IO(targetErrorMessages)
    }

    for {
      _ <- testConsole.feedLines("will.109")
      _ <- FunctionalHello.program
      lines <- testConsole.output
      errorLines <- testConsole.errorOutput
    } yield {
      lines shouldBe Vector("enter your name: ", "Hello will.109")
      errorLines shouldBe Vector[String]()
    }
  }
}
