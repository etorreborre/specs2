package org.specs2
package specification

import org.specs2.Specification
import org.specs2.matcher.MustMatchers
import org.specs2.execute.{StandardResults, Function0Result, Success}
import Function0Result._
import org.specs2.main.Arguments
import org.specs2.specification.create._
import org.specs2.specification.core._
import org.specs2.specification.dsl.FragmentDsl

class S2StringContextSpec extends Specification { def is = s2"""

 Fragments can be interpolated from a s2 string
  a simple string as Text, aggregating it to the previous Text ${exs.e1}
  another fragment                                             ${exs.e2}
  a result                                                     ${exs.e3}
  a delayed result                                             ${exs.e4}
  fragments                                                    ${exs.e5}
  fragments from a specification                               ${exs.e6}
  2 examples                                                   ${exs.e7}
  an example and a tag                                         ${exs.e8}
  an example and a section                                     ${exs.e9}
  """

}

object exs extends MustMatchers with StandardResults {
  import interpolator._

  val env = Env()

  def e1 = s2"""this is ${"some text"}""".fragments.fragments must haveSize(1)

  def e2 = s2"""this is ${DefaultFragmentFactory.Text("some text")}""".fragments.fragments must haveSize(2)

  def e3 = s2"""this is $ok""".fragments.fragments must haveSize(1)

  def e4 = s2"""this is ${new Function0Result(() => Success())}""".fragments.fragments must haveSize(1)

  def e5 = s2"""this is ${Fragments(Text("the"), Text(" world"))}""".fragments.fragments must haveSize(3)

  def e6 = s2"""this is $spec""".fragments.fragments must haveSize(3)

  def e7 = s2"""
  this should
    create example 1 $ok
    create example 2 $ok""".fragments.fragments must haveSize(4)

  def e8 = s2"""a tagged example $ok ${DefaultFragmentFactory.TaggedAs("x")}""".fragments.fragments.map(Fragment.fragmentType) must_==
    Seq("Example", "Tag", "Text")

  def e9 = s2"""a tagged example $ok ${DefaultFragmentFactory.AsSection("x")}""".fragments.fragments.map(Fragment.fragmentType) must_==
    Seq("Example", "Tag", "Text")

  val spec = new SpecificationStructure { outer =>
    def is = SpecStructure(SpecHeader(outer.getClass), Arguments(), Fragments(Text("the"), Text(" world")))
  }

  val ok = Success()
}

trait interpolator extends S2StringContext with DelegatedFragmentFactory {
  implicit class specificationInStringContext3(sc: StringContext) {
    def s3(variables: InterpolatedPart*): SpecStructure = macro S2Macro.s2Implementation
  }
}

object interpolator extends interpolator

trait dsl1 extends interpolator with FragmentDsl
object dsl1 extends dsl1