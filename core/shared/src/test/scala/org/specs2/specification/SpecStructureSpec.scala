package org.specs2
package specification

import core._
import control._
import main._

class SpecStructureSpec(env: Env) extends Specification { def is = s2"""

 A spec structure depends on another if it has links to it $a1

 Spec structures can be built from SpecificationRefs
  they must keep their arguments $a2

 We can get all the linked specifications according to tags $b1

"""

  lazy val s1 = S1.is
  lazy val s2 = S2.is

  def a1 =
    SpecStructure.dependsOn(s1, s2) and SpecStructure.dependsOn(s2, s1).not

  def a2 =
    SpecStructure.linkedSpecifications(s1, env, getClass.getClassLoader).runOption.flatMap(_.lastOption) must beSome((_: SpecStructure).arguments must_== s2.arguments)

  def b1 =
    SpecStructure.linkedSpecifications(s1, env.setArguments(Arguments.split("exclude s2")),getClass.getClassLoader).
      runOption.toList.flatten must not(contain((s: SpecStructure) => s.name === "S2"))
}

object S1 extends Specification { def is = link(xonly ^ S2.is) ^ tag("s2") }
object S2 extends Specification { def is = "text" ^ tag("s2")}
