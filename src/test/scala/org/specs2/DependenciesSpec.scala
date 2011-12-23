package org.specs2

import specification._

class DependenciesSpec extends Specification with Analysis with Tags { def is = noindent ^ section("slow") ^
                                                                                          """
  The following dependencies must be enforced in specs2:
                                                                                          """ ^
  layers (
    "runner",
    "reporter",
                                    // this class is excluded because it contains its own runner
    "specification mutable".exclude("mutable.SpecificationWithJUnit"),
    "mock      form",
    "matcher",
    "execute",
    "reflect analysis time html",
    "collection control io xml text main data").withPrefix("org.specs2")                  ^ section("slow") ^
                                                                                          end

}