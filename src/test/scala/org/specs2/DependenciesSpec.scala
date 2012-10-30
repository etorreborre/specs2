package org.specs2

import specification._

class DependenciesSpec extends Specification with Analysis with Tags { def is = noindent                                ^
                                                                                                                        """
  The following dependencies must be enforced in specs2:
                                                                                                                        """ ^
  layers (
    "runner",                                             // specification runners, usually just taking the class name
    "reporter",                                           // execute and report the specification as html, console text, junit xml,....
                                                          // Specification structure: Fragments, Context, Statistics, Tags
                                                          // this class is excluded because it contains its own runner
    "mutable".exclude("mutable.SpecificationWithJUnit"),  // mutable classes relies on non-mutable ones
    "specification",
    "mock      form",                                     // mocks definition/checks and forms definitions
    "matcher",                                            // matchers and expectations
    "execute",                                            // execution of anything that returns a Result
    "reflect analysis time html",                         // support classes for reflection, time measurement,...
    "collection control io xml text json main data",      // utility classes for io, main arguments, control structures,...
  ).withPrefix("org.specs2")                                                                                            ^
                                                                                                                        end

}