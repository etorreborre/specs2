package org.specs2
package matcher

import cats.data.NonEmptyList

class NonEmptyListMatchersSpec extends Spec with NonEmptyListMatchers { def is =
  s2"""
      The NonEmptyListMatchers allows matching on the size of NonEmptyLists

      hasSize() checks the the NonEmptyList's length is equal to the expected value
      ${ NonEmptyList.of("single value") must haveSize(1) }
      ${ NonEmptyList.of("first", "second", "third") must haveSize(3) }
    """
}