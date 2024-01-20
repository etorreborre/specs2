package org.specs2.control

import org.specs2.*
import org.specs2.fp.*
import org.specs2.fp.syntax.*
import io.*
import scala.collection.mutable.ListBuffer

class OperationSpec extends Specification {
  def is = s2"""

  Guarding an operation throwing an exception $guard
  
  The finalizers of an operation must be executed when the operation is executed
    with a flatmap $finalizers1
    with a simple applicative $finalizers2
    with a more complex applicative $finalizers3

"""

  def guard =
    val operation = FileSystem(NoLogger).readFile(FilePath("missing")).orElse(Operation.ok("ok"))
    operation.runOption === Some("ok")


  def finalizers1 =
    val messages = ListBuffer[String]();
    def add(m: String) = messages.addOne(m)
    val operation = Operation.delayed(add("action1")).flatMap(_ => Operation.delayed(add("action2"))).thenFinally(Operation.delayed(add("final")))
    val result = operation.runOption
    result === Some(List("action1", "action2", "final"))

  def finalizers2 =
    val messages = ListBuffer[String]();
    def add(m: String) = messages.addOne(m)

    val o1 = Operation.delayed(add("operation1")).thenFinally(Operation.delayed(add("final1")))
    val o2 = Operation.delayed(add("operation2")).thenFinally(Operation.delayed(add("final2")))
    val result = (o1 *> o2).runOption.get.toList

    // all effects are executed, including finalizers
    result === List("operation1", "operation2", "final1", "final2")

  def finalizers3 =
    val messages = ListBuffer[String]();
    def add(m: String) = messages.addOne(m)

    val o1 = Operation.delayed(add("o1-1")).flatMap(_ => Operation.delayed(add("o1-2"))).thenFinally(Operation.delayed(add("final1")))
    val o2 = Operation.delayed(add("o2-1")).flatMap(_ => Operation.delayed(add("o2-2"))).thenFinally(Operation.delayed(add("final2")))
    val result = Applicative[Operation].tuple2(o1, o2).runOption.get._1.toList

    // all effects are executed, including finalizers
    result === List("o1-1", "o1-2", "o2-1", "o2-2", "final1", "final2")

}
