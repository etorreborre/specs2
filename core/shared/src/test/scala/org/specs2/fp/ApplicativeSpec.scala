package org.specs2
package fp

import syntax._
import control._
import scala.collection.mutable._
import eff._

class ApplicativeSpec extends Specification { def is = s2"""

Applicative effects must be evaluated in the right order
  with ap2 $ap2Order
  with ap3 $ap3Order
  with ap4 $ap4Order
  with ap5 $ap5Order
  with ap6 $ap6Order
  with ap7 $ap7Order
  with ap8 $ap8Order
  a list must be traversed in the right order $listTraverse

"""

  val applicative = Eff.EffApplicative[OperationStack]
  import applicative._

  def ap2Order = {
    val evaluated = new ListBuffer[Int]
    runOperation(ap2(doIt(1, evaluated), doIt(2, evaluated))(Operations.ok((_:Int,_:Int) => 0)))
    evaluated.toList === List(1, 2)
  }

  def ap3Order = {
    val evaluated = new ListBuffer[Int]
    runOperation(ap3(doIt(1, evaluated), doIt(2, evaluated), doIt(3, evaluated))(Operations.ok((_:Int,_:Int,_:Int) => 0)))
    evaluated.toList === List(1, 2, 3)
  }

  def ap4Order = {
    val evaluated = new ListBuffer[Int]
    runOperation(ap4(doIt(1, evaluated), doIt(2, evaluated), doIt(3, evaluated), doIt(4, evaluated))(Operations.ok((_:Int,_:Int,_:Int,_:Int) => 0)))
    evaluated.toList === List(1, 2, 3, 4)
  }

  def ap5Order = {
    val evaluated = new ListBuffer[Int]
    runOperation(ap5(doIt(1, evaluated), doIt(2, evaluated), doIt(3, evaluated), doIt(4, evaluated),
      doIt(5, evaluated))(Operations.ok((_:Int,_:Int,_:Int,_:Int,_:Int) => 0)))
    evaluated.toList === List(1, 2, 3, 4, 5)
  }

  def ap6Order = {
    val evaluated = new ListBuffer[Int]
    runOperation(ap6(doIt(1, evaluated), doIt(2, evaluated), doIt(3, evaluated), doIt(4, evaluated),
      doIt(5, evaluated), doIt(6, evaluated))(Operations.ok((_:Int,_:Int,_:Int,_:Int,_:Int,_:Int) => 0)))
    evaluated.toList === List(1, 2, 3, 4, 5, 6)
  }

  def ap7Order = {
    val evaluated = new ListBuffer[Int]
    runOperation(ap7(doIt(1, evaluated), doIt(2, evaluated), doIt(3, evaluated), doIt(4, evaluated),
      doIt(5, evaluated), doIt(6, evaluated), doIt(7, evaluated))(Operations.ok((_:Int,_:Int,_:Int,_:Int,_:Int,_:Int,_:Int) => 0)))
    evaluated.toList === List(1, 2, 3, 4, 5, 6, 7)
  }

  def ap8Order = {
    val evaluated = new ListBuffer[Int]
    runOperation(ap8(doIt(1, evaluated), doIt(2, evaluated), doIt(3, evaluated), doIt(4, evaluated),
      doIt(5, evaluated), doIt(6, evaluated), doIt(7, evaluated), doIt(8, evaluated))(Operations.ok((_:Int,_:Int,_:Int,_:Int,_:Int,_:Int,_:Int,_:Int) => 0)))
    evaluated.toList === List(1, 2, 3, 4, 5, 6, 7, 8)
  }

  def listTraverse = {
    val values = List(1, 2, 3)
    val evaluated = new ListBuffer[Int]
    runOperation(values.traverse(i => doIt(i, evaluated))) === Right(values)

    evaluated.toList === values
  }

  /* HELPERS */
  def doIt(i: Int, evaluated: ListBuffer[Int]): Operation[Int] =
    Operations.delayed {evaluated.append(i); i }

}
