package examples

import org.specs2._
import specification.Tags

/**
 * There are several ways to select the examples to execute:
 *
 *  - by using arguments and a regular expression matching on the example name
 *  - by using tags
 *  - by using sections
 *
 * For more details @see org.specs2.guide.Structure
 */
class SelectExamplesSpec extends Specification {
  /**
   * This specification uses the `only` argument to only execute some examples.
   *
   * It only executes the first example
   */
  class IncludeSpec extends Specification { def is = only("ex.*1") ^ s2"""
    This is a list of examples
      example1                                                     $success
      example2                                                     $success
                                                                   """
  }

  /**
   * same thing as the previous one but with a mutable specification
   */
  class IncludeMutableSpec extends mutable.Specification {
    only("ex.*1")
    "This is a list of examples" >> {
      "example1"                 >> success
      "example2"                 >> success
    }
  }

  /**
   * This specification uses the `Tags` trait to tag the second example and the 'include' argument
   * to select it
   */
  class TagsSpec extends Specification with Tags { def is = include("unit") ^  s2"""
    This is a list of examples
      example1                                                     $success
      example2                                                     $success ${tag("unit")}
                                                                   """
  }


  /**
   * Same thing in a mutable spec, using the `mutable.Tags` trait.
   *
   * Note that the tag has to be on the same line as the example otherwise it applies to the next one!
   */
  class TagsMutableSpec extends mutable.Specification with mutable.Tags {
    include("unit")

    "This is a list of examples" >> {
      "example1"                 >> success tag ("unit")
      "example2"                 >> success
    }
  }

  /**
   * This specification uses the `Tags` trait and the `section` method to tag the examples 2, 3 and 4
   */
  class SectionSpec extends Specification with Tags { def is = include("unit") ^ s2"""
    This is a list of examples
      example1                                                     $success
      example2                                                     $success ${section("unit")}
      example3                                                     $success
      example4                                                     $success ${section("unit")}
      example5                                                     $success ${tag("unit")}
                                                                   """
  }


  /**
   * Same thing in a mutable specification
   */
  class SectionMutableSpec extends mutable.Specification with mutable.Tags {
    include("unit")

    "This is a list of examples" >> {
      "example1"                 >> success
      "example2"                 >> success section ("unit")
      "example3"                 >> success
      "example4"                 >> success section ("unit")
      "example5"                 >> success
    }
  }

  def is = include(new IncludeSpec,
                   new IncludeMutableSpec,
                   new TagsSpec,
                   new TagsMutableSpec,
                   new SectionSpec,
                   new SectionMutableSpec)
}