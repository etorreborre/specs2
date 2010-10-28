package org.specs2
package matcher
import execute._

trait MatchersImplicits {
  
  /** 
   * implicit definition to accept any boolean value as a Result
   * This avoids writing b must beTrue 
   */ 
  implicit def toResult(b: Boolean): Result = {
    new BeTrueMatcher().apply(new Expectable(b)).toResult
  }
}
object MatchersImplicits extends MatchersImplicits
