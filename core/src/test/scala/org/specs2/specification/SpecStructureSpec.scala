package org.specs2
package specification

import main.Arguments
import core._
import control._

class SpecStructureSpec extends Specification {
  def is = s2"""

 A spec structure depends on another if it has links to it $a1

 Spec structures can be built from SpecificationRefs
  they must keep their arguments $a2

"""

  def a1 = {
    lazy val s1 = S1.is
    lazy val s2 = S2.is
    SpecStructure.dependsOn(s1, s2) and SpecStructure.dependsOn(s2, s1).not
  }

  def a2 = { env: Env =>
    lazy val s1 = S1.is
    lazy val s2 = S2.is
    SpecStructure.linkedSpecifications(s1, env, getClass.getClassLoader).runOption.flatMap(_.lastOption) must beSome((_: SpecStructure).arguments must_== s2.arguments)

  }
}

object S1 extends Specification { def is = link(xonly ^ S2.is) }
object S2 extends Specification { def is = "text" }
