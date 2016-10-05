package org.specs2
package control

import eff.NoFx

package object origami {

  object fold extends FoldFunctions

  /** alias for a non-effectful Fold */
  type FoldId[A, U] = Fold[NoFx, A, U]

  /** alias for a non-effectful Fold where the state type is U */
  type FoldState[A, B] = Fold[NoFx, A, B] { type S = B }

  /** alias for a Fold sinking its last value */
  type Sink[R, A] = Fold[R, A, Unit]

  /** alias for a Fold exposing it state type */
  type Aux[R, A, B, S1] = Fold[R, A, B] { type S = S1 }

}

