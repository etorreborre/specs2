package org.specs2
package matcher

import execute._

private[specs2]
trait MatchersImplicits {
  
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
