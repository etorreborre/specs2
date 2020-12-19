package org.specs2
package execute

import Result.ResultFailureMonoid
import text.Quote._
import text.Plural._
import org.specs2.fp.syntax._
import control._

/**
 * This trait adds some implicits to easily fold sequences of results
 */
trait ResultImplicits extends ResultLogicalCombinators:

  extension [T, R : AsResult](t: T => R)

    /** apply the function to the value and convert to a Result */
    def applied(value: T): Result =
      AsResult(t(value))

  extension [T, R : AsResult](t: T => R)

    /** @return the "and" of all results, stopping after the first failure */
    def forall(seq: Traversable[T]): Result =
      if seq.isEmpty then
        StandardResults.success
      else
        val (index, r): (Int, Result) = seq.drop(1).foldLeft((0, t.applied(seq.head))) { case ((i, res), cur) =>
          if AsResult(res).isSuccess then
            (i + 1, t.applied(cur))
          else
            (i, res)
        }
        val res = AsResult(r)

        lazy val failingElementMessage: String =
          "In the sequence "+qseq(seq)+" the "+(index+1).th+" element is failing: "+res.message

        if res.isSuccess then
          Success("All elements of "+qseq(seq)+" are successful")
        else
          Failure(failingElementMessage)

    /**
     * @return the aggregation of all results
     */
    def foreach(seq: Traversable[T]): Result =
      if seq.isEmpty then
        StandardResults.success
      else
        seq.drop(1).foldLeft(t.applied(seq.head)) { (res, cur) => AsResult(res) |+| t.applied(cur) }

    /**
     * @return success if at least one result is a success
     */
    def atLeastOnce(seq: Traversable[T]) =
      if seq.isEmpty then
        Failure("no result")
      else
        seq.drop(1).foldLeft(t.applied(seq.head)) { (r, cur) =>
          val res = AsResult(r)
          if res.isSuccess then res else t.applied(cur)
        }

  /**
   * Two results r1 and r2 are equivalent if
   * r1.isSuccess == r2.isSuccess
   */
  extension [R1 : AsResult, R2 : AsResult](r1: =>R1)
    def <==>(r2: =>R2): Result =
      val (result1, result2) = (AsResult(r1), AsResult(r2))
      if result1.isSuccess == result2.isSuccess then
        Success()
      else
        Failure(result1.message+"\nis not equivalent to\n"+result2.message)

object ResultImplicits extends ResultImplicits
