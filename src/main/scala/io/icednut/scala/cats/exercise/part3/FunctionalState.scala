package io.icednut.scala.cats.exercise.part3

object FunctionalState {

  type MyState[S, A] = S => (S, A) // S: State, A: Answer (Single computation)

  import cats.data.State

  val countAndSay: State[Int, String] = State(currentCount => (currentCount + 1, s"Counted $currentCount"))
  val (eleven, counted10) = countAndSay.run(10).value
  // state = "iterative" computations

  // iterative
  var a = 10
  a += 1
  val firstComputation = s"Added 1 to 10, obtained $a"
  a *= 5
  val secondComputation = s"Multiplied with 5, obtained $a"

  // pure FP with states
  var firstTransformation = State((s: Int) => (s + 1, s"Added 1 to 10, obtained ${s + 1}"))
  val secondTransformation = State((s: Int) => (s * 5, s"Multiplied with 5, obtained ${s * 5}"))
  val compositeTransformation: State[Int, (String, String)] = firstTransformation.flatMap { firstResult =>
    secondTransformation.map(secondResult => (firstResult, secondResult))
  }
  val compositeTransformation2 = for {
    firstResult <- firstTransformation
    secondResult <- secondTransformation
  } yield (firstResult, secondResult)

  val func1 = (s: Int) => (s + 1, s"Added 1 to 10, obtained ${s + 1}")
  val func2 = (s: Int) => (s * 5, s"Multiplied with 5, obtained ${s * 5}")
  val compositeResult = func1.andThen {
    case (newState, firstResult) => (firstResult, func2(newState))
  }

  // TODO: an online store
  case class ShoppingCart(items: Vector[String], total: Double)

  def addToCart(item: String, price: Double): State[ShoppingCart, Double] = State[ShoppingCart, Double](shoppingCart => (
    ShoppingCart(items = shoppingCart.items :+ item, total = shoppingCart.total + price),
    shoppingCart.total + price
  ))

  // TODO 2: pure mental gymnastics
  def inspect[A, B](f: A => B): State[A, B] = State { v =>(v, f(v))}
  def get[A]: State[A, A] = State { v =>(v, v)}
  def set[A](value: A): State[A, Unit] = State { _ => (value, ())}
  def modify[A](f: A => A): State[A, Unit] = State{ v => (f(v), ())}

  // methods available
  import cats.data.State._

  def main(args: Array[String]): Unit = {
    println(compositeTransformation.run(10).value)
    println(compositeTransformation2.run(20).value)
    println(compositeResult(10))

    val totalPrice = for {
      _ <- addToCart("Jackson Guitar", 100.0)
      secondResult <- addToCart("MacbookAir", 3000.0)
    } yield {
      secondResult
    }
    println(s"total price: ${totalPrice.run(ShoppingCart(Vector.empty[String], 0.0)).value}")

    val myState = for {
      _ <- set[String]("Hello")
      _ <- set[String]("Hi")
      _ <- modify[String](result => result + " world!")
      result <- get[String]
    } yield result
    println(s"MyState: ${myState.run("").value}")
  }
}
