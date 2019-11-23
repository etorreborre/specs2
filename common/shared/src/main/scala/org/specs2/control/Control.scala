package org.specs2
package control

import Action._
import fp._
import producer._
import Transducer._

object Control {

  type AsyncStream[A] = Producer[A]
  type AsyncTransducer[A, B] = Transducer[A, B]

  type AsyncFold[A, B] = origami.Fold[Action, A, B]
  type AsyncSink[A] = origami.Fold[Action, A, Unit]

  implicit val idToAction: NaturalTransformation[Id, Action] =
    NaturalTransformation.naturalId[Action]

  def emitAsync[A](as: A*): AsyncStream[A] =
    Producer.emitSeq(as)

  def emitAsyncDelayed[A](a: A): AsyncStream[A] =
    Producer.eval(Action.protect(a))

}
