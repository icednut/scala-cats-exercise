package io.icednut.scala.cats.exercise.part3

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Writers {

  import cats.data.Writer

  // 1 - define them at the start
  val aWriter: Writer[List[String], Int] = Writer(List("Started something"), 45)

  // 2 - manipulate them with pure FP
  val anIncreasedWriter = aWriter.map(_ + 1) // value increases, logs stay the same
  val aLogsWriter = aWriter.mapWritten(_ :+ "found something interesting") // value stays the same, logs change
  val aWriterWithBoth = aWriter.bimap(_ :+ "found something interesting", _ + 1) // both value and logs change
  val aWriterWithBoth2 = aWriter.mapBoth { (logs, value) =>
    (logs :+ "found something interesting", value + 1)
  }

  // flatMap

  import cats.instances.vector._ // imports a Semigroup[Vector]

  val writerA = Writer(Vector("Log A1", "Log A2"), 10)
  val writerB = Writer(Vector("Log B1"), 40)
  val compositeWriter = for {
    va <- writerA
    vb <- writerB
  } yield va + vb

  // reset the logs

  import cats.instances.list._ // a implicit monoid[List[Int]]

  val anEmptyWriter = aWriter.reset // clear the logs, keep the value

  // 3 - dump either the value or the logs
  val desiredValue = aWriter.value
  val logs = aWriter.written
  val (l, v) = aWriter.run

  // TODO 1 : rewrite a function which "prints" things with writers
  def countAndSay(n: Int): Unit = {
    if (n <= 0) println("starting!")
    else {
      countAndSay(n - 1)
      println(n)
    }
  }

  // my answer
  //  def countAndLog(n: Int): Writer[Vector[String], Int] =
  //    Writer((1 to n).toVector, n)
  //      .mapWritten(logs => "starting!" +: logs.map(_.toString))
  def countAndLog(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector("starting!"), 0)
    else countAndLog(n - 1).flatMap(_ => Writer(Vector(s"$n"), n))
  }
  // Benefit #1: we work with pure FP

  // TODO 2
  def naiveSum(n: Int): Int = {
    if (n <= 0) 0
    else {
      println(s"Now at $n")
      val lowerSum = naiveSum(n - 1)
      println(s"Computed sum(${n - 1}) = $lowerSum")
      lowerSum + n
    }
  }

  def naiveSumAndLog(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) {
      Writer.value(n)
    } else {
      for {
        _ <- Writer(Vector(s"Now at $n"), n)
        currentLogWriter <- naiveSumAndLog(n - 1)
        nextNumber <- Writer(Vector(s"Computed sum(${n - 1}) = $currentLogWriter"), currentLogWriter + n)
      } yield {
        nextNumber
      }
    }
  }

  def sumWithLogs(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) {
      Writer(Vector(), 0)
    } else {
      for {
        _ <- Writer(Vector("Not at $n"), n)
        lowerSum <- sumWithLogs(n - 1)
        _ <- Writer(Vector(s"Computed sum(${n - 1}) = $lowerSum"), n)
      } yield {
        lowerSum + n
      }
    }
  }
  // Benefit #2: Writers can keep logs separate on multiple threads

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  def main(args: Array[String]): Unit = {
    println(compositeWriter.run)
    println(anEmptyWriter.run)
    println(countAndSay(10))
    println("-------------------")
    println(countAndLog(10).run)
    println("-------------------")
    println(countAndLog(10).written.foreach(println))
    println("-------------------")
    Future(naiveSum(10)).foreach(println)
    Future(naiveSum(10)).foreach(println)
    println("=============================")
    //    println(naiveSumAndLog(10).written.foreach(println))
    //    println(sumWithLogs(10).written.foreach(println))
    val sumFuture1 = Future(naiveSumAndLog(10))
    val sumFuture2 = Future(naiveSumAndLog(10))
    sumFuture1.map(_.written).foreach(println)
    sumFuture2.map(_.written).foreach(println)
  }
}
