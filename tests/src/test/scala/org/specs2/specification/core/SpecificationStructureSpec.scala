package org.specs2
package specification
package core

import org.junit.runner.RunWith
import org.scalacheck.*
import Gen.*
import Arbitrary.*
import org.specs2.runner.JUnitRunner

import scala.IllegalArgumentException
import fp.syntax.*
import matcher.*
import OperationMatchers.*

class SpecificationStructureSpec(val env: Env) extends Specification with ScalaCheck with OwnEnv { def is = s2"""

 There can be links between specifications and it is possible to sort all the dependent specifications
 so that
   the first dependencies come first $sort
   the order of links in a given specification is preserved $linksOrder

 A specification structure can be created from a class name
   if there is an exception during the creation of the specification instance
   it must be reported as the cause for the instantiation issue $report
   if the companion object is not a specification structure, try the normal class $companion
   if the companion object is a specification structure keep it $companionSpec

"""

  def sort = prop { (specification: SpecificationStructure) =>
    val linked = SpecificationStructure.linkedSpecifications(specification, env, getClass.getClassLoader).runMonoid
    val sorted = SpecificationStructure.topologicalSort(env)(linked).getOrElse(Vector()).map(_.structure)

    sorted must contain { (s: SpecStructure) =>
      val (before, after) = sorted.splitAt(sorted.indexOf(s))
      before must contain((b: SpecStructure) => b must not(dependOn(s))).forall
    }.forall
  }.set(maxSize = 5)

  def linksOrder = prop { (links: List[Fragment]) =>
    val specification = new SpecificationStructure { def is = SpecStructure.create(SpecHeader.create(getClass), Fragments(links*)) }
    val linked = SpecificationStructure.linkedSpecifications(specification, env, getClass.getClassLoader).runMonoid
    val sorted = SpecificationStructure.topologicalSort(env)(linked).get.map(_.structure)

    sorted.dropRight(1).map(_.specClassName) must ===(specification.structure.linkReferencesList.map(_.specClassName).distinct)

  }.setArbitrary(ArbitraryLinks).set(maxSize = 5)

  def report =
    SpecificationStructure.create("org.specs2.specification.core.BrokenSpecification").runOperation must beLeft((t: Throwable) =>
       t.getCause.getCause.getMessage === "boom")

  def companion =
    SpecificationStructure.create("org.specs2.specification.core.SpecWithCompanion", getClass.getClassLoader, None).
      // the cast is necessary to really show that the right instance has been built
      // see #477
      map(_.asInstanceOf[SpecificationStructure]) must
      beOk

  def companionSpec =
    SpecificationStructure.create("org.specs2.specification.core.SpecObject", getClass.getClassLoader, None).
      // the cast is necessary to really show that the right instance has been built
      // see #477
      map(_.asInstanceOf[SpecificationStructure]) must
      beOk

  def dependOn(s2: SpecStructure): Matcher[SpecStructure] = (s1: SpecStructure) =>
    (s1.dependsOn(s2)(ee), s"${s1.specClassName} doesn't depend on ${s2.specClassName}")

  given Arbitrary[SpecificationStructure] =
    Arbitrary(arbitrary[SpecStructure].map(spec => new SpecificationStructure { def is = spec }))

  given Arbitrary[SpecStructure] = Arbitrary {
    (ArbitrarySpecHeader.arbitrary |@| ArbitraryFragments.arbitrary)((sh, fs) => SpecStructure.create(sh, fs))
  }

  given ArbitraryFragments: Arbitrary[Fragments] =
    Arbitrary(listOf(ArbitraryFragment.arbitrary).map(fs => Fragments.apply(fs*)))

  given ArbitraryFragment: Arbitrary[Fragment] =
    Arbitrary(Gen.oneOf(Arbitraries.FragmentArbitrary.arbitrary, ArbitraryLink.arbitrary))

  given ArbitraryLinks: Arbitrary[List[Fragment]] =
    Arbitrary(Gen.nonEmptyListOf(ArbitraryLink.arbitrary))

  given ArbitraryLink: Arbitrary[Fragment] =
    Arbitrary(arbitrary[SpecHeader].map(ss => link(SpecStructure(ss))))

  given ArbitrarySpecHeader: Arbitrary[SpecHeader] =
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

@RunWith(classOf[JUnitRunner])
class SpecWithCompanion extends Specification { def is = s2""" """ }
object SpecWithCompanion

object SpecObject extends Specification { def is = s2""" """ }
