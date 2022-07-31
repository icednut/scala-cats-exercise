package io.icednut.scala.cats.exercise.part2

object Monoids {

  import cats.Semigroup
  import cats.instances.int._
  import cats.syntax.semigroup._ // import the |+| extension method

  val numbers = (1 to 100000).toList
  // |+| is always associative
  val sumLeft = numbers.foldLeft(0)(_ |+| _)
  val sumRight = numbers.foldRight(0)(_ |+| _)

  // define a general API
//    def combineFoldWithSemigroup[T](list: List[T])(implicit semigroup: Semigroup[T]): T =
//      list.foldLeft(???)(_ |+| _)
  // Semigroup is not enough to provide a starting value here.
  // There is no way that semigroup would be capable enough to give us a good starting value of any kind T.

  // Type class could provide an empty value for any kind type T as called a Monoid.

  // MONOIDS

  import cats.Monoid

  val intMonoid = Monoid[Int]
  val combineInt = intMonoid.combine(23, 999)
  val zero = intMonoid.empty // 0
  // Monoids are different semigroups in the sense that they can also provide a starting value depending on their type.

  import cats.instances.string._ // bring the implicit Monoid[String] in scope

  val emptyString = Monoid[String].empty
  val combineString = Monoid[String].combine("I understand ", "monoids")

  import cats.instances.option._ // construct an implicit Monoid[Option[Int]]

  val emptyOption = Monoid[Option[Int]].empty
  val combineOption = Monoid[Option[Int]].combine(Option(2), Option.empty[Int])
  val combineOption2 = Monoid[Option[Int]].combine(Option(3), Option(6))

  // extension method for Monoids: |+|
  val combinedOptionFancy = Option(3) |+| Option(7)

  // TODO 1: implement a reduceByFold
  def combineFold[T](list: List[T])(implicit monoid: Monoid[T]): T =
    list.foldLeft(monoid.empty)(_ |+| _)

  // TODO 2: combine a list of phonebooks as Maps[String, Int]
  val phonebooks = List(
    Map(
      "Alice" -> 235,
      "Bob" -> 647
    ),
    Map(
      "Charlie" -> 372,
      "Deniel" -> 889
    ),
    Map(
      "Tina" -> 123
    )
  )
  import cats.instances.map._
  val onePhonebook = combineFold(phonebooks)

  // TODO 3 - shopping cart and online stores with Monoids
  // hint: define your monoid - Monoid.instance
  case class ShoppingCart(items: List[String], total: Double) // Don't use concurrency.
//  implicit val shoppingCartMonoid: Monoid[ShoppingCart] = new Monoid[ShoppingCart] {
//    override def empty: ShoppingCart = ShoppingCart(items = List.empty[String], total = 0.0)
//
//    override def combine(x: ShoppingCart, y: ShoppingCart): ShoppingCart =
//      ShoppingCart(items = x.items ++ y.items, total = x.total + y.total)
//  }
  implicit val shoppingCartMonoid = Monoid.instance[ShoppingCart](
    ShoppingCart(items = List.empty[String], total = 0.0),
    (x, y) => ShoppingCart(items = x.items ++ y.items, total = x.total + y.total)
  )
  def checkout(shoppingCarts: List[ShoppingCart]): ShoppingCart =
    combineFold(shoppingCarts)

  def main(args: Array[String]): Unit = {
    println(s"sumLeft: $sumLeft")
    println(s"sumRight: $sumRight")

    println(s"emptyString: $emptyString")
    println(s"combineOption: $combineOption")
    println(s"combinedOptionFancy: $combinedOptionFancy")
    println(s"[combineFold] ${combineFold((1 to 1000).toList)}")
    println(s"[combineFold] ${combineFold(List("Hello, ", "cats ", "and ", "scala."))}")
    println(s"[combineFold] ${combineFold((1 to 1000).map(n => Option(n)).toList)}")
    println(s"onePhonebook: $onePhonebook")
    println(s"checkout: ${checkout(
      shoppingCarts = List(
        ShoppingCart(items = List("Pencil", "Eraser", "Colored Paper"), total = 20.99),
        ShoppingCart(items = List("MacBook", "Keyboard", "Mouse"), total = 9999.99),
        ShoppingCart(items = List("Book 1", "Book 2", "Book 3"), total = 799.99),
        ShoppingCart(items = List(), total = 0.0)
      )
    )}")
  }

  // 정리: Monoid is Natural extension of Semigroups that can offer a "zero" value.
}
