package io.icednut.scala.cats.exercise.part3

import io.icednut.scala.cats.exercise.part3.Evaluation.reverseList

object Evaluation {

  /*
    Cats makes the distinction between
   */

  import cats.Eval

  val instantEval: Eval[Int] = Eval.now {
    println("Computing now!")
    64345
  }

  val redoEval = Eval.always {
    print("Computing again! ")
    4234
  }

  val delayedEval = Eval.later {
    print("Computing later! ")
    53278
  }

  val composedEvaluation = instantEval.flatMap(value1 => delayedEval.map(value2 => value1 + value2))
  val composedEvaluationFor = for {
    value1 <- instantEval
    value2 <- delayedEval
  } yield value1 + value2 // identical

  // TODO 1: predict the output
  val evalEx1 = for {
    a <- delayedEval
    b <- redoEval
    c <- instantEval
    d <- redoEval
  } yield a + b + c + d
  // "Computing now!"
  // "Computing later! "
  // "Computing again! "
  // "Computing again! "

  // "remember" a computed value
  val dontRecompute = redoEval.memoize

  val tutorial = Eval
    .always {
      println("Step 1...")
      "put guitar on your Lap"
    }
    .map { step1 =>
      println("Step 2");
      s"$step1 then your left hand on the neck"
    }
    .memoize
    .map { steps12 =>
      println("Step 3, more complicated")
      s"$steps12 then with the right hand strike the strings"
    }

  // TODO 2: implement defer such that defer(Eval.now) does NOT run the side effects
  //  def defer[T](expr: => Eval[T]): Eval[T] = Eval.defer(expr)
  def defer[T](expr: => Eval[T]): Eval[T] = Eval.later(()).flatMap(_ => expr)

  // TODO 3: rewrite the method with Evals
  def reverseList[T](list: List[T]): List[T] =
    if (list.isEmpty) list
    else reverseList(list.tail) :+ list.head

//    def reverseEval[T](list: List[T]): Eval[List[T]] =
//      if (list.isEmpty) Eval.later(list)
//      else for {
//        reverseList <- reverseEval(list.tail)
//        head <- Eval.later(list.head)
//      } yield {
//        reverseList :+ head
//      }

  def reverseEval[T](list: List[T]): Eval[List[T]] =
    if (list.isEmpty) Eval.now(list)
    else Eval.defer(reverseEval(list.tail).map(_ :+ list.head))

  def main(args: Array[String]): Unit = {
    //    println(s"instantEval.value == ${instantEval.value}")
    //    println(s"instantEval.value == ${instantEval.value}")
    //    println(s"redoEval.value == ${redoEval.value}")
    //    println(s"redoEval.value == ${redoEval.value}")
    //    println(s"delayedEval.value == ${delayedEval.value}")
    //    println(s"delayedEval.value == ${delayedEval.value}")
    //    println(s"composedEvaluation.value == ${composedEvaluation.value}")
    //    println(s"composedEvaluation.value == ${composedEvaluation.value}")
    //    println(s"composedEvaluationFor.value == ${composedEvaluation.value}")
    //    println(s"evalEx1.value == ${evalEx1.value}")
    //    println(s"evalEx1.value == ${evalEx1.value}")
    //    println(s"dontRecompute.value == ${dontRecompute.value}")
    //    println(tutorial.value)
    //    println(tutorial.value)
    val deferEval = defer(Eval.now {
      println("Now!")
      42
    })
    //    println(deferEval.value)

    val listEval = reverseEval((1 to 10000).toList)
    println(listEval.value)
  }
}
