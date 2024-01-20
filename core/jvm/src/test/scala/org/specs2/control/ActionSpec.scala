package org.specs2.control

import org.specs2.*
import org.specs2.fp.*
import scala.collection.mutable.*
import org.specs2.concurrent.ExecutionEnv
import scala.concurrent.*
import scala.concurrent.duration.*

class ActionSpec(using ee: ExecutionEnv) extends Specification {
  def is = s2"""

  The finalizers of an action must be executed when the action is run as a future - with flatMap $finalizers1
  The finalizers of an action must be executed when the action is run as a future - with ap $finalizers2

"""

  def finalizers1 =
    val messages = ListBuffer[String]();
    def add(m: String) = messages.addOne(m)
    // val action =
    //   Action.pure(add("action1")).flatMap(_ => Action.pure(add("action2"))).thenFinally(Finalizer.create(add("final")))
    // val result = Await.result(action.runFuture(ee), Duration.Inf)
    // result.toList === List("action1", "action2", "final")
    val a1 =
      Action.pure(add("a1-1")).flatMap(_ => Action.pure(add("a1-2"))).thenFinally(Finalizer.create(add("final1")))
    val a2 =
      Action.pure(add("a2-1")).flatMap(_ => Action.pure(add("a2-2"))).thenFinally(Finalizer.create(add("final2")))
    val result = a1.flatMap(a1 => a2.map(a2 => (a1, a2))).runOption(ee).get._1.toList

    // all effects are executed, including finalizers
    result.sorted === List("a1-1", "a2-1", "a1-2", "a2-2", "final1", "final2").sorted

  def finalizers2 =
    val messages = ListBuffer[String]();
    def add(m: String) = messages.addOne(m)

    val a1 =
      Action.pure(add("a1-1")).flatMap(_ => Action.pure(add("a1-2"))).thenFinally(Finalizer.create(add("final1")))
    val a2 =
      Action.pure(add("a2-1")).flatMap(_ => Action.pure(add("a2-2"))).thenFinally(Finalizer.create(add("final2")))
    val result = Applicative[Action].tuple2(a1, a2).runOption(ee).get._1.toList

    // all effects are executed, including finalizers
    result.sorted === List("a1-1", "a2-1", "a1-2", "a2-2", "final1", "final2").sorted

}
