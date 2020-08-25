package org.specs2
package control

import fp._

package object origami:

  /** alias for a non-effectful Fold */
  type FoldId[A, U] = Fold[Id, A, U]

  /** alias for a non-effectful Fold where the state type is U */
  type FoldState[A, B] = Fold[Id, A, B] { type S = B }

  /** alias for a Fold sinking its last value */
  type Sink[M[_], A] = Fold[M, A, Unit]

  /** alias for a Fold with async actions */
  type AsyncFold[A, B] = Fold[Action, A, B]

  /** alias for a Sink with async actions */
  type AsyncSink[A] = Fold[Action, A, Unit]

