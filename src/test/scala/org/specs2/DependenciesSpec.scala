package org.specs2

import specification._

class DependenciesSpec extends Specification with Analysis with Tags { def is = noindent ^ section("slow") ^
                                                                                          """
  The following dependencies must be enforced in specs2:
                                                                                          """ ^
  layers (
    "runner",                                  // specification runners, usually just taking the class name
    "reporter",                                // execute and report the specification as html, console text, junit xml,....
                                               // Specification structure: Fragments, Context, Statistics, Tags
                            // this class is excluded because it contains its own runner
    "specification mutable".exclude("mutable.SpecificationWithJUnit"),
    "mock      form",                          // mocks definition/checks and forms definitions
    "matcher",                                 // matchers and expectations
    "execute",                                 // execution of anything that returns a Result
    "reflect analysis time html",              // support classes for reflection, time measurement,...
    "collection control io xml text main data" // utility classes for io, main arguments, control structures,...
  ).withPrefix("org.specs2")                  ^ section("slow") ^
                                                                                          end

}