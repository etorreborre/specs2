package org.specs2.matcher
import org.specs2.mutable.Specification

import cats.data.Validated.{Valid, Invalid}

class ValidatedMatchersSpec extends Specification with ValidatedMatchers with ResultMatchers {

  "ValidatedMatchersSpec".br.tab(1)

  "beValid" should {
    "match on a Valid instance" in {
      Valid(1) must beValid
    }

    "match on a Valid instance with value check" in {
      Valid(1) must beValid(1)

      Valid(1) must beValid.like { case i => i must be_>(0)}

      Valid(1) must beValid((i: Int) => i must be_>(0))
    }

    "not match on an Invalid instance" in {
      (Invalid(1) must beValid(1)) must beFailing
    }

    "not match on a Valid instance with non-matching expression" in {
      (Valid(0) must beValid((i: Int) => i must be_>(0))) must beFailing
    }
  }

  "beInvalid" should {
    "match on an Invalid instance" in {
      Invalid(1) must beInvalid
    }

    "match on an Invalid instance with value check" in {
      Invalid(1) must beInvalid(1)

      Invalid(1) must beInvalid.like { case i => i must be_>(0)}

      Invalid(1) must beInvalid((i: Int) => i must be_>(0))
    }

    "not match on a Valid instance" in {
      (Valid(1) must beInvalid(1)) must beFailing
    }

    "not match on an Invalid instance with non-matching expression" in {
      (Invalid(0) must beInvalid((i: Int) => i must be_>(0))) must beFailing
    }
  }
}
