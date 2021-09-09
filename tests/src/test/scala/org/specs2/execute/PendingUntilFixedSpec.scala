package org.specs2
package execute

import org.scalacheck.Prop.forAll
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

// see issue #987
class PendingUntilFixedSpec extends Specification with ScalaCheck {
  "Pending until fixed should" >> {
    "work with ScalaCheck in a mutable spec" >> forAll((_: Int) => failure("boom!")).pendingUntilFixed
  }
}
