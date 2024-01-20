package org.specs2
package runner

import matcher.ActionMatchers
import specification.core.*

class SpecFactorySpec(env: Env) extends Specification with ActionMatchers {
  def is = s2"""

 A spec factory instantiates specifications from names
   from object names            $objects1
   from object names without $$ $objects2
   from classes                 $classes
"""

  def objects1 =
    SpecFactory.create(env).createSpecification("org.specs2.runner.RunnerSpecification$").runOption must beSome

  def objects2 =
    SpecFactory.create(env).createSpecification("org.specs2.runner.RunnerSpecification").runOption must beSome

  def classes =
    SpecFactory.create(env).createSpecification("org.specs2.runner.SpecFactorySpec").runOption must beSome

}

object RunnerSpecification extends Specification { def is = success }
