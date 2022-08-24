package io.icednut.scala.cats.exercise.part4

import cats.Monoid

object Folding {

  // TODO - implement all in terms of foldLeft & foldRight
  object ListExercises {
    def map[A, B](list: List[A])(f: A => B): List[B] = list.foldLeft[List[B]](List.empty[B]) { (nextList: List[B], element: A) =>
      nextList :+ f(element)
    }
    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = list.foldRight[List[B]](List.empty[B]) { (element: A, nextList: List[B]) =>
      f(element) ++ nextList
    } // TODO 이거 map으로 푸는 방법이 있지 않았나?
    def filter[A](list: List[A])(predicate: A => Boolean): List[A] = list.foldLeft[List[A]](List.empty[A]) { (nextList: List[A], element: A) =>
      if (predicate(element)) {
        nextList :+ element
      } else {
        nextList
      }
    }
    def combineAll[A](list: List[A])(implicit monoid: Monoid[A]): A = list.foldLeft[A](monoid.empty) { (element: A, sum: A) =>
      monoid.combineAll(List(element, sum))
    }
  }

  def main(args: Array[String]): Unit = {
    implicit val intMonoid = Monoid[Int]

    println(ListExercises.map((1 to 10).toList)(_ + 1))
    println(ListExercises.flatMap((1 to 10).toList)(v => List(v + 1)))
    println(ListExercises.filter((1 to 10).toList)(_ > 5))
    println(ListExercises.combineAll((1 to 10).toList))
  }
}
