package user.io

import org.specs2.mutable.Specification

/**
 * This specification is used to check if the line numbers are correct when creating fragments locations
 */
class LocationUnitSpecification extends Specification {
  "this block" should {
    "have one example" in ok
    "have another example" in ko
  }

  "this other block should" >> {
    "have one ok example" >>
      ok
    "have one ko example" >>
      ko
  }
}
