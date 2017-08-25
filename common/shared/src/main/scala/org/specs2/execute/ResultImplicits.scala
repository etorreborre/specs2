package org.specs2
package execute

import Result.ResultFailureMonoid
import text.Quote._
import text.Plural._
import org.specs2.fp.syntax._
/**
 * This trait adds some implicits to easily fold sequences of results
 */
trait ResultImplicits {

  implicit def verifyResultFunction[T, R : AsResult](t: T => R): ResultFunctionVerification[T, R] =
    new ResultFunctionVerification(t)

  class ResultFunctionVerification[T, R : AsResult](t: T => R) {

    /** apply the function to the value and convert to a Result */
    def apply(value: T) = AsResult(t(value))

    /** @return the "and" of all results, stopping after the first failure */
    def forall[S <: Traversable[T]](seq: S) = {
      if (seq.isEmpty) StandardResults.success
      else {
        val (index, r) = seq.drop(1).foldLeft((0, apply(seq.head))) { case ((i, res), cur) =>
          if (res.isSuccess) (i + 1, apply(cur))
          else               (i, res)
        }
        lazy val failingElementMessage = "In the sequence "+qseq(seq)+" the "+(index+1).th+" element is failing: "+r.message

        if (r.isSuccess) Success("All elements of "+qseq(seq)+" are successful")
        else             Failure(failingElementMessage)
      }
    }

    /**
     * @return the aggregation of all results
     */
    def foreach[S <: Traversable[T]](seq: S) = {
      if (seq.isEmpty) StandardResults.success
      else             seq.drop(1).foldLeft(apply(seq.head)) { (res, cur) => res |+| apply(cur) }
    }

    /**
     * @return success if at least one result is a success
     */
    def atLeastOnce[S <: Traversable[T]](seq: S) = {
      if (seq.isEmpty) Failure("no result")
      else seq.drop(1).foldLeft(apply(seq.head)) { (res, cur) => if (res.isSuccess) res else apply(cur) }
    }
  }

  /**
   * Two results r1 and r2 are equivalent if
   * r1.isSuccess == r2.isSuccess
   */
  implicit class resultsEquivalence[R1 : AsResult](r1: =>R1) {
    def <==>[R2 : AsResult](r2: =>R2): Result = {
      val (result1, result2) = (AsResult(r1), AsResult(r2))
      ResultLogicalCombinators.combineResult(Success()).iff(result1.isSuccess == result2.isSuccess).mapMessage { _ =>
        result1.message+"\nis not equivalent to\n"+result2.message
      }
    }
  }

}

object ResultImplicits extends ResultImplicits
