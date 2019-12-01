package org.specs2
package control

import Action._
import fp._
import producer._


object Control {

  implicit val idToAction: NaturalTransformation[Id, Action] =
    NaturalTransformation.naturalId[Action]

  implicit val operationToAction: NaturalTransformation[Operation, Action] =
    new NaturalTransformation[Operation, Action] {
      def apply[A](operation: Operation[A]): Action[A] =
        operation.toAction
    }

  def emitAsync[A](as: A*): AsyncStream[A] =
    Producer.emitSeq(as)

  def emitAsyncDelayed[A](a: A): AsyncStream[A] =
    Producer.eval(Action.protect(a))

}
