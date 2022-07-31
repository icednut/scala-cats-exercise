package io.icednut.scala.cats.exercise.part2

object Functors {

  // a higher kinded type of Functor

  // TODO 1
  trait Tree[+T]

  object Tree {
    // smart constructors
    def leaf[T](value: T) = Leaf(value)

    def branch[T](value: T, left: Tree[T], right: Tree[T]): Tree[T] = Branch(value, left, right)
  }

  case class Leaf[+T](value: T) extends Tree[T]

  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]


  import cats.Functor

  // My Solution
  //  implicit val treeFunctor = new Functor[Tree]() {
  //    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
  //      case Leaf(v) => Leaf(value = f(v))
  //      case Branch(v, left, right) => Branch(value = f(v), left = map(left)(f), right = map(right)(f))
  //    }
  //  }

  // Solution
  implicit object TreeFunctor extends Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Leaf(v) => Leaf(value = f(v))
      case Branch(v, left, right) => Branch(value = f(v), left = map(left)(f), right = map(right)(f))
    }
  }

  //  def do10x[T](container: Tree[T])(implicit functor: Functor[Tree]): Tree[T] = functor.map(container)(_ * 10)
  //  def do10x(container: Tree[Int])(implicit functor: Functor[Tree]): Tree[Int] = functor.map(container)(_ * 10)
  def do10x[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] = functor.map(container)(_ * 10)

  // extension method - map

  import cats.syntax.functor._

  val tree: Tree[Int] = Tree.branch(40, Tree.branch(5, Tree.leaf(10), Tree.leaf(30)), Tree.leaf(20))
  val incrementedTree = tree.map(_ + 1)

  // TODO 2: write a shorted do10x method using extension methods
  //  def do10xShorter[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] = container.map(_ * 10)
  def do10xShorter[F[_] : Functor](container: F[Int]): F[Int] = container.map(_ * 10)

  def main(args: Array[String]): Unit = {
    println(do10x(
      Tree.branch(
        value = 10,
        left = Tree.branch(
          value = 29,
          left = Tree.leaf(15),
          right = Tree.leaf(288)
        ),
        right = Tree.leaf(25)
      )
    ))
    println(do10xShorter(
      Tree.branch(
        value = 10,
        left = Tree.branch(
          value = 29,
          left = Tree.leaf(15),
          right = Tree.leaf(288)
        ),
        right = Tree.leaf(25)
      )
    ))
  }
}
