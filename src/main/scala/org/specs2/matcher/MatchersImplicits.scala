package org.specs2
package matcher

import execute._

/**
 * This trait provides implicit definitions from MatchResults and Booleans to Results
 */
private[specs2]
trait MatchersImplicits {
  /** 
   * implicit definition to transform a Seq of MatchResults to a Result
   */ 
  implicit def seqToResult[T](r: Seq[MatchResult[T]]): Result = r.reduceLeft(_ and _).toResult
  /** 
   * implicit definition to transform any MatchResult to a Result
   */ 
  implicit def asResult[T](r: MatchResult[T]): Result = r.toResult
  
  /** 
   * implicit definition to accept any boolean value as a Result
   * This avoids writing b must beTrue 
   */ 
  implicit def toResult(b: Boolean): Result = {
    new BeTrueMatcher().apply(Expectable(b)).toResult
  }
}
private[specs2]
object MatchersImplicits extends MatchersImplicits
