package io.icednut.scala.cats.exercise.part5

import cats.Monoid

object ContravariantFunctors {

  trait Format[T] {
    self => // contravariant type class
    def format(value: T): String

    def contramap[A](func: A => T): Format[A] = new Format[A] {
      override def format(value: A): String = self.format(func(value))
    }
  }

  def format[A](value: A)(implicit format: Format[A]): String = format.format(value)

  implicit object StringFormat extends Format[String] {
    override def format(value: String): String = "\"" + value + "\""
  }

  implicit object IntFormat extends Format[Int] {
    override def format(value: Int): String = value.toString
  }

  implicit object BooleanFormat extends Format[Boolean] {
    override def format(value: Boolean): String = if (value) "Y" else "N"
  }

  // bad
  //  implicit def getOptionFormat[T](implicit f: Format[T]): Format[Option[T]] = new Format[Option[T]] {
  //    override def format(value: Option[T]): String = f.format(value.get)
  //  }

  // good
  implicit def getOptionFormat[T](implicit f: Format[T], mo: Monoid[T]): Format[Option[T]] = f.contramap[Option[T]](_.getOrElse(mo.empty))

  /*
    IntFormat
    fo: Format[Option[Int]] = IntFormat.contramap[Option[Int]](_.get) // first get
    fo2: Format[Option[Option[Int]]] = fo.contramap[Option[Option[Int]]](_.get) // second get
   */

  import cats.Contravariant
  import cats.Show
  import cats.instances.int._

  val showInts = Show[Int]
  val showOption: Show[Option[Int]] = Contravariant[Show].contramap(showInts)(_.getOrElse(0))

  import cats.syntax.contravariant._
  val showOption2: Show[Option[Int]] = showInts.contramap(_.getOrElse(0))

  def main(args: Array[String]): Unit = {
    println(format("Nothing weird so far"))
    println(format(45))
    println(format(true))
    println(format(Option(45)))
    println(format(Option(Option(45))))
    println(showOption2.show(None))
  }
}
