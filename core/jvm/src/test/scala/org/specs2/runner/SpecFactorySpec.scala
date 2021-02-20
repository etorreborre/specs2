package org.specs2
package runner

import matcher.ActionMatchers

class SpecFactorySpec extends Specification with ActionMatchers { def is = s2"""

 A spec factory instantiates specifications from names
   from object names            $objects1
   from object names without $$ $objects2
   from classes                 $classes
"""

  def objects1 =
    SpecFactory.default.createSpecification("org.specs2.runner.RunnerSpecification$").runOption `must` beSome

  def objects2 =
    SpecFactory.default.createSpecification("org.specs2.runner.RunnerSpecification").runOption `must` beSome

  def classes =
    SpecFactory.default.createSpecification("org.specs2.runner.SpecFactorySpec").runOption `must` beSome

}

object RunnerSpecification extends Specification { def is = success }
