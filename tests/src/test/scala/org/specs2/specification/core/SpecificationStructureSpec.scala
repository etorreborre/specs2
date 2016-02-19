package org.specs2
package specification
package core

import org.scalacheck._, Gen._, Arbitrary._
import scala.IllegalArgumentException
import scalaz._, Scalaz._
import control._, eff.ErrorEffect
import matcher._

class SpecificationStructureSpec extends Specification with ScalaCheck with DisjunctionMatchers { def is = s2"""

 There can be links between specifications and it is possible to sort all the dependent specifications
 so that
   the first dependencies come first $sort
   the order of links in a given specification is preserved $linksOrder

 A specification structure can be created from a class name
   if there is an exception during the creation of the specification instance
   it must be reported as the cause for the instantiation issue $report

"""

  def sort = (env: Env) => prop { specification: SpecificationStructure =>
    val linked = SpecificationStructure.linkedSpecifications(specification, env, getClass.getClassLoader).runOption.getOrElse(List())
    val sorted = SpecificationStructure.topologicalSort(env)(linked).getOrElse(Vector()).map(_.structure(env))

    sorted must contain { s: SpecStructure =>
      val (before, after) = sorted.splitAt(sorted.indexOf(s))
      before must contain((b: SpecStructure) => b must not(dependOn(s))).forall
    }.forall
  }.set(maxSize = 5)

  def linksOrder = (env: Env) => prop { links: List[Fragment] =>
    val specification = new SpecificationStructure { def is = SpecStructure.create(SpecHeader.create(getClass), Fragments(links:_*)) }
    val linked = SpecificationStructure.linkedSpecifications(specification, env, getClass.getClassLoader).runOption.getOrElse(List())
    val sorted = SpecificationStructure.topologicalSort(env)(linked).get.map(_.structure(env))

    sorted.dropRight(1).map(_.specClassName) must_== specification.structure(env).linkReferences.map(_.specClassName)

  }.setArbitrary(ArbitraryLinks).set(maxSize = 5)

  def report = {
    runAction(SpecificationStructure.create("org.specs2.specification.core.BrokenSpecification")) must be_-\/((e: ErrorEffect.Error) =>
      e must be_-\/((e1: Throwable) => e1.getCause.getCause.getMessage === "boom")
    )
  }

  def dependOn(s2: SpecStructure): Matcher[SpecStructure] = (s1: SpecStructure) =>
    (s1 dependsOn s2, s"${s1.specClassName} doesn't depend on ${s2.specClassName}")

  implicit def ArbitrarySpecificationStructure: Arbitrary[SpecificationStructure] =
    Arbitrary(arbitrary[SpecStructure].map(spec => new SpecificationStructure { def is = spec }))

  implicit def ArbitrarySpecStructure: Arbitrary[SpecStructure] = Arbitrary {
    (ArbitrarySpecHeader.arbitrary |@| ArbitraryFragments.arbitrary)((sh, fs) => SpecStructure.create(sh, fs))
  }

  implicit def ArbitraryFragments: Arbitrary[Fragments] =
    Arbitrary(listOf(ArbitraryFragment.arbitrary).map(fs => Fragments.apply(fs:_*)))

  implicit def ArbitraryFragment: Arbitrary[Fragment] =
    Arbitrary(Gen.oneOf(Arbitraries.FragmentArbitrary.arbitrary, ArbitraryLink.arbitrary))

  implicit def ArbitraryLinks: Arbitrary[List[Fragment]] =
    Arbitrary(Gen.nonEmptyListOf(ArbitraryLink.arbitrary))

  implicit def ArbitraryLink: Arbitrary[Fragment] =
    Arbitrary(arbitrary[SpecHeader].map(ss => link(SpecStructure(ss))))

  implicit def ArbitrarySpecHeader: Arbitrary[SpecHeader] =
    Arbitrary(Gen.oneOf(Seq(SS1, SS2, SS3, SS4, SS4).map(s => SpecHeader.create(s.getClass))))
}

object SS1 extends Specification { def is = "" }
object SS2 extends Specification { def is = "" }
object SS3 extends Specification { def is = "" }
object SS4 extends Specification { def is = "" }
object SS5 extends Specification { def is = "" }

class BrokenSpecification extends Specification { def is = s2"""
  test$ok
"""

  throw new IllegalArgumentException("boom")

}
