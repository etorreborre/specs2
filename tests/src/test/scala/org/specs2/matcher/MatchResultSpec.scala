package org.specs2
package matcher

import execute._
import org.scalacheck._, Arbitrary._, Gen._
import text.Sentences._

class MatchResultSpec extends Specification with ScalaCheck { def is = s2"""

 A list of match results can be sequenced 
   the result of the sequence is the fold of all the results   $fold
   the expectable of the sequence contains all the expectables $expectables

 A MatchResult can be muted
   its message must be empty $mute
   there must be no failure details $muteFailure


"""

  def fold = prop { (results: List[MatchResult[Int]]) =>
    MatchResult.sequence(results).toResult.isSuccess must_== results.map(_.toResult).foldLeft(success: Result)(_ and _).isSuccess
  }

  def expectables = prop { (results: List[MatchResult[Int]]) =>
    MatchResult.sequence(results).expectable.value must_== results.map(_.expectable.value)
  }

  def mute = prop { result: MatchResult[Int] =>
    result.mute.message must beEmpty
  }

  def muteFailure = prop { result: MatchFailure[Int] =>
    result.mute.details must_== NoDetails
  }

  implicit def MatchResultArbitrary[T : Arbitrary]: Arbitrary[MatchResult[T]] = Arbitrary {
    for {
      t <- arbitrary[T]
      b <- arbitrary[Boolean]
      m <- arbitrary[String]
      e =  createExpectable(t)
      r <- Gen.oneOf(
        Matcher.result(b, m, negateSentence(m), e),
        MatchPending(m, e),
        MatchSkip(m, e))
    } yield r
  }

  implicit def MatchFailureArbitrary[T : Arbitrary]: Arbitrary[MatchFailure[T]] = Arbitrary {
    for {
      t <- arbitrary[T]
      m <- arbitrary[String]
      d <- arbitrary[Details]
    } yield MatchFailure(() => m, () => negateSentence(m), createExpectable(t), details = d)
  }

  implicit def DetailsArbitrary: Arbitrary[Details] = Arbitrary {
    Gen.oneOf[Details](
      NoDetails,
      FailureDetails("abc", "bca"),
      FailureSeqDetails(Seq(1, 2), Seq(3, 4)),
      FailureSetDetails(Set(1, 2), Set(3, 4)),
      FailureMapDetails(Map(1 -> 2), Map(3 -> 4)))
  }
}
