package io.icednut.scala.cats.exercise.console

import cats.effect.std.Console
import cats.effect.{ExitCode, IO, IOApp, Sync}

object FunctionalHello extends IOApp.Simple {

  def program(implicit console: Console[IO]): IO[Unit] = {
    for {
      _ <- Console[IO].print("enter your name: ")
      name <- Console[IO].readLine
      _ <- Console[IO].print(s"Hello $name")
    } yield ()

  }

  override def run: IO[Unit] = program

}
