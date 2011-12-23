package org.specs2

import specification.Analysis

class DependenciesSpec extends Specification with Analysis { def is = noindent ^
                                                                                          """
  The following dependencies must be enforced in specs2:
                                                                                          """ ^
  layers (
    "runner",
    "reporter",
    "specification mutable".exclude("mutable.SpecificationWithJUnit"),
    "mock      form",
    "matcher",
    "execute",
    "reflect analysis time html",
    "collection control io xml text main data").withPrefix("org.specs2")                  ^
                                                                                          end

}