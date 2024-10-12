package user

import org.specs2.*
import org.specs2.concurrent.*
import org.specs2.control.*
import org.specs2.control.origami.*
import org.specs2.control.producer.*
import org.specs2.fp.*

class FoldSpec(ee: ExecutionEnv) extends Specification {
  def is = s2"""

  A correct implementation of NaturalTransformation from Id to Action must not duplicate effects (see issue #1277) $dontDuplicateEffects

"""

  def dontDuplicateEffects = {
    val p = Producer.emitAll[Action, Int](1, 2, 3)

    // The NaturalTransformation from Id (which is Id[X] X) to Action must
    // make sure to not run side effects of X twice.
    // This tests guarantees that during the evaluation of `zip` and the various folds
    // we don't run the action for collection elements into a mutable map more than necessary.
    val vs = p.fold(Folds.list[Int].into[Action] `zip` Folds.list[Int].into[Action])
    vs.run(ee) === (List(1, 2, 3), List(1, 2, 3))
  }
}
