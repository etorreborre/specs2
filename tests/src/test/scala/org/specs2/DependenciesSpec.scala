package org.specs2

import specification._

class DependenciesSpec extends Specification with Analysis { def is = s2"""

  The following dependencies must be enforced in specs2:

  ${layers (
     "runner",
     "reporter",
     "mutable".exclude("mutable.SpecificationWithJUnit"),
     "specification form",
     "mock",
     "matcher",
     "execute",
     "reflect analysis time html",
    "collection control io xml text json main data"
   ).withPrefix("org.specs2") }
                                                                                                                        """

}