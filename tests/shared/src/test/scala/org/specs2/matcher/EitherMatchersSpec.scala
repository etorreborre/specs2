package org.specs2
package matcher

class EitherMatchersSpec extends Spec with EitherMatchers with ResultMatchers {
  def is = s2"""

 The EitherMatchers trait provides matchers to check Either instances

  beRight checks if an element is Right(_)
  ${Right(1) must beRight(1)}
  ${Right(1) must beRight((i: Int) => i must be_>(0))}
  ${Right(1) must beRight(Seq(true, true))}
  ${Right(1) must beRight(===(1))}
  ${Right(Seq(1)) must beRight(===(Seq(1)))}
  ${Left(1) must not(beRight(1))}
  ${Right(1) must beRight.like { case i => i must be_>(0) }}
  ${(Right(1) must beRight.like { case i =>
      i must be_<(0)
    }) returns "Right(1) is Right but 1 is greater or equal than 0"}

  beLeft checks if an element is Left(_)
  ${Left(1) must beLeft(1)}
  ${Left(1) must beLeft(===(1))}
  ${Right(1) must not(beLeft(1))}
  ${Left(1) must beLeft.like { case i => i must be_>(0) }}

  beRight / beLeft must typecheck when composed with other matchers
  ${val boomException: Throwable = new Exception("boom")
    Some(Left(boomException)) must beSome(beLeft(boomException))
    }

"""

}
