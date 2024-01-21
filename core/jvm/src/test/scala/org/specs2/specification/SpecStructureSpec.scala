package org.specs2
package specification

import core.*
import main.*
import org.specs2.concurrent.ExecutionEnv

class SpecStructureSpec(val env: Env) extends Specification with OwnEnv:
  def is = s2"""

 A spec structure depends on another if it has links to it $a1

 Spec structures can be built from SpecificationRefs
  they must keep their arguments $a2

 We can get all the linked specifications according to tags $b1

"""

  lazy val spec1 = S1.is
  lazy val spec2 = S2.is

  def a1 =
    SpecStructure.dependsOn(ee)(spec1, spec2) and SpecStructure.dependsOn(ee)(spec2, spec1).not

  def a2 =
    SpecStructure
      .linkedSpecifications(spec1, ownEnv, getClass.getClassLoader)
      .runOption
      .flatMap(_.lastOption) must beSome(
      (_: SpecStructure).arguments
        must ===(spec2.arguments)
    )

  def b1 =
    SpecStructure
      .linkedSpecifications(spec1, ownEnv.setArguments(Arguments.split("exclude spec2")), getClass.getClassLoader)
      .runOption
      .toList
      .flatten must not(contain((s: SpecStructure) => s.name === "S2"))

object S1 extends Specification { def is = link(xonly ^ S2.is) ^ tag("s2") }
object S2 extends Specification { def is = "text" ^ tag("s2") }
