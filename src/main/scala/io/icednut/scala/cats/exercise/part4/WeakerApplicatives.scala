package io.icednut.scala.cats.exercise.part4

import cats.{Functor, Semigroupal}

object WeakerApplicatives {

  //  trait MyApplicative[W[_]] extends Functor[W] with Semigroupal[W] {
  //    def pure[A](x: A): W[A]
  //
  //    override def product[A, B](fa: W[A], fb: W[B]) = {
  //      val functionWrapper: W[B => (A, B)] = map(fa)(a => (b: B) => (a, b))
  //      ap(functionWrapper)(fb)
  //    }
  //
  //    def ap[W[_], B, T](wf: W[B => T])(wb: W[B]): W[T]
  //  }

  trait MyApply[W[_]] extends Functor[W] with Semigroupal[W] {
    override def product[A, B](fa: W[A], fb: W[B]): W[(A, B)] = {
      val functionWrapper: W[B => (A, B)] = map(fa)(a => (b: B) => (a, b))
      ap(functionWrapper)(fb)
    }

    // TODO
    def mapN[A, B, C](tuple: (W[A], W[B]))(f: (A, B) => C): W[C] = {
      val productedValue: W[(A, B)] = product(tuple._1, tuple._2)
//      map[(A, B), C](productedValue)((v: (A, B)) => f(v._1, v._2))
      map(productedValue) {
        case (a, b) => f(a, b)
      }
    }

    def ap[W[_], B, T](wf: W[B => T])(wb: W[B]): W[T] // funamental
  }

  trait MyApplicative[W[_]] extends MyApply[W] {
    def pure[A](x: A): W[A]
  }

  import cats.Apply
  import cats.instances.option._ // implicit Apply[Option]

  val applyOption = Apply[Option]
  val funcApp = applyOption.ap(Some((x: Int) => x + 1))(Some(2)) // Some(3)

  import cats.syntax.apply._ // extension methods from Apply

  val tupleOfOptions = (Option(1), Option(2), Option(3))
  val optionOfTuple = tupleOfOptions.tupled // Some((1, 2, 3))
  val sumOption = tupleOfOptions.mapN(_ + _ + _) // Some(6)

  def main(args: Array[String]): Unit = {

  }
}
