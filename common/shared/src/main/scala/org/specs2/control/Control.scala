package org.specs2
package control

import Action1._
import fp._
import producer._, Transducer1._

object Control {

  type AsyncStream[A] = Producer1[A]
  type AsyncTransducer[A, B] = Transducer1[A, B]

  type AsyncFold[A, B] = origami.Fold[Action1, A, B]
  type AsyncSink[A] = origami.Fold[Action1, A, Unit]

  implicit val idToAction1: NaturalTransformation[Id, Action1] =
    NaturalTransformation.naturalId[Action1]

  def emitAsync[A](as: A*): AsyncStream[A] =
    producer.producers1.emitSeq(as)

  def emitAsyncDelayed[A](a: A): AsyncStream[A] =
    producer.producers1.eval(Action1.protect(a))

}
