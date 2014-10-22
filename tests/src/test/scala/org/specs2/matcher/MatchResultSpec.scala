package org.specs2
package matcher

import execute._
import org.scalacheck._
import Gen._

class MatchResultSpec extends Specification with ScalaCheck { def is = s2"""

 A list of match results can be sequenced 
   the result of the sequence is the fold of all the results   $fold
   the expectable of the sequence contains all the expectables $expectables

"""

  def fold = prop { (results: List[MatchResult[Int]]) =>
    MatchResult.sequence(results).toResult.isSuccess must_== results.map(_.toResult).foldLeft(success: Result)(_ and _).isSuccess
  }

  def expectables = prop { (results: List[MatchResult[Int]]) =>
    MatchResult.sequence(results).expectable.value must_== results.map(_.expectable.value)
  }

  implicit def MatchResultArbitrary[T : Arbitrary]: Arbitrary[MatchResult[T]] = Arbitrary {
    for {
      t <- implicitly[Arbitrary[T]].arbitrary
      b <- Arbitrary.arbBool.arbitrary
    } yield Matcher.result(b, "ok", "ko", createExpectable(t))
  }
}
