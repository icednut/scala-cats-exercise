package io.icednut.scala.cats.exercise.part2

import cats.implicits.toFlatMapOps

import scala.annotation.tailrec

object CustomMonads {

  import cats.Monad

  implicit object OptionMonad extends Monad[Option] {

    override def pure[A](x: A): Option[A] = Option(x)

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

    // tailrecM does not stack-overflow
    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = f(a) match {
      case Some(Left(v)) => tailRecM(v)(f)
      case Some(Right(b)) => Option(b)
      case _ => None
    }
  }

  // TODO 1: define a monad for the identity type
  type Identity[T] = T
  val aNumber: Identity[Int] = 42

  implicit object IdentityMonad extends Monad[Identity] {

    override def pure[A](x: A): Identity[A] = x

    override def flatMap[A, B](fa: Identity[A])(f: A => Identity[B]): Identity[B] = f(fa)

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Identity[Either[A, B]]): Identity[B] = f(a) match {
      case Left(v) => tailRecM(v)(f)
      case Right(b) => b
    }
  }

  // harder example
  sealed trait Tree[+A]
  final case class Leaf[+A](value: A) extends Tree[A]
  final case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

  // TODO 2: define a monad for this Tree
  // tailRecM tailrec is difficult
  implicit object TreeMonad extends Monad[Tree] {

    override def pure[A](x: A): Tree[A] = Leaf(x)
    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Leaf(v) => f(v)
      case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
    }
    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      def run(tree: Tree[Either[A, B]]): Tree[B] = {
        tree match {
          case Leaf(Left(v)) => tailRecM(v)(f)
          case Leaf(Right(b)) => Leaf(b)
          case Branch(left, right) => Branch(run(left), run(right))
        }
      }

      def tailRec(todo: List[Tree[Either[A, B]]], expanded: Set[Tree[Either[A, B]]], done: List[Tree[B]]): Tree[B] =
        if (todo.isEmpty) done.head
        else todo.head match {
          case Leaf(Left(v)) => tailRec(f(v) :: todo.tail, expanded, done)
          case Leaf(Right(b)) => tailRec(todo.tail, expanded, Leaf(b) :: done)
          case node@Branch(left, right) =>
            if (!expanded.contains(node)) {
              tailRec(right :: left :: todo, expanded + node, done)
            } else {
              val newLeft = done.head
              val newRight = done.tail.head
              val newBranch = Branch(newLeft, newRight)

              tailRec(todo.tail, expanded, newBranch :: done.drop(2))
            }
        }

//      run(f(a))
      tailRec(List(f(a)), Set(), List())
    }
  }

  def main(args: Array[String]): Unit = {
    val tree: Tree[Int] = Branch(
      left = Leaf(10),
      right = Leaf(20)
    )
//    val changedTree = TreeMonad.flatMap(tree)(v => Branch(Leaf(v + 1), Leaf(v + 2)))
    val changedTree = tree.flatMap(v => Branch(Leaf(v + 1), Leaf(v + 2)))

    println(changedTree)
  }
}
