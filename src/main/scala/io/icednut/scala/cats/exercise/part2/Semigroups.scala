package io.icednut.scala.cats.exercise.part2

object Semigroups {

  // Semigroups COMBINE elements of the same type

  import cats.Semigroup
  import cats.instances.int._

  val naturalIntSemigroup = Semigroup[Int]
  val intCombination = naturalIntSemigroup.combine(1, 2)

  import cats.instances.string._

  val naturalStringSemigroup = Semigroup[String]
  val stringCombination = naturalStringSemigroup.combine("I love ", "Cats")

  def reduceInts(list: List[Int]): Int = list.reduce(naturalIntSemigroup.combine)

  def reduce(list: List[String]): String = list.reduce(naturalStringSemigroup.combine)

  // general API
  def reduceThings[T](list: List[T])(implicit semigroup: Semigroup[T]): T = list.reduce(semigroup.combine)

  // TODO 1: support a new type
  // hint: use the same pattern we used with Eq
  case class Expense(id: Long, amount: Double)

  //  implicit val naturalExpenseSemigroup: Semigroup[Expense] = new Semigroup[Expense] {
  //    override def combine(x: Expense, y: Expense): Expense = Expense(x.id, x.amount + y.amount)
  //  }
  implicit val expenseSemigroup = Semigroup.instance[Expense] { (e1, e2) =>
    Expense(id = Math.max(e1.id, e2.id), amount = e1.amount + e2.amount)
  }

  // extension methods from Semigroup - |+|

  import cats.syntax.semigroup._

  val anIntSum = 2 |+| 3
  //  val anIntSum = 2 |+| "Hello" // compile error
  val aStringConcat = "we like " |+| "semigroups"
  //  val aStringConcat = "we like " |+| 2 // compile error
  val aCombinedExpense = Expense(3, 80) |+| Expense(56, 44)

  // TODO 2: implement reduceThings2
//  def reduceThings2[T](list: List[T])(implicit semigroup: Semigroup[T]): T = list.reduce(_ |+| _)
  def reduceThings2[T : Semigroup](list: List[T]): T = list.reduce(_ |+| _)

  def main(args: Array[String]): Unit = {
    // step 1
    println(intCombination)
    println(stringCombination)

    // step 2
    val numbers = (1 to 10).toList
    println(reduceInts(numbers))

    val strings = List("I love ", "Cats")
    println(reduce(strings))

    // step 3
    println(reduceThings(numbers)) // compiler injects the implicit Semigroup[Int]
    println(reduceThings(strings)) // compiler injects the implicit Semigroup[String]

    // step 4
    import cats.instances.option._
    // compiler will produce an implicit Semigroup[Option[Int]] - combine will produce another option with the summed elements
    // compiler will produce an implicit Semigroup[Option[String]] - combine will produce another option with the concatenated elements
    // same for any type with an implicit Semigroup
    val numberOptions = numbers.map(n => Option(n))
    println(reduceThings(numberOptions))

    val stringOptions = strings.map(s => Option(s))
    println(reduceThings(stringOptions))

    // step 5
    val expenses = List(Expense(1, 1000), Expense(2, 4000))
    println(reduceThings(expenses))

    val expenseOptions = expenses.map(e => Option(e))
    println(reduceThings(expenseOptions))

    // step 6
    println(reduceThings2(numberOptions))
    println(reduceThings2(stringOptions))
    println(reduceThings2(expenses))
    println(reduceThings2(expenseOptions))
  }
}
