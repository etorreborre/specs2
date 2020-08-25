package org.specs2
package control

package object producer:

  /** Transformation of a Producer into another one: after filtering, mapping, etc... */
  type Transducer[F[_], A, B] = Producer[F, A] => Producer[F, B]

  /** Producer with async actions */
  type AsyncStream[A] = Producer[Action, A]

  /** Transducer with async actions */
  type AsyncTransducer[A, B] = Transducer[Action, A, B]

