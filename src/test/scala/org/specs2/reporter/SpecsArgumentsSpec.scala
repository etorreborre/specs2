package org.specs2
package reporter

import org.scalacheck.{ Arbitrary, Gen, Prop }
import org.scalacheck.Arbitrary._
import scalaz._
import Scalaz._
import main.Arguments
import matcher._
import specification._
import SpecsArguments._
import FragmentSpecsArgumentsReducer._

class SpecsArgumentsSpec extends SpecificationWithJUnit with ScalazMatchers with ArbitraryFragments { def is =
                                                                                                       """
The Specs Arguments class maps a seq of fragments to their applicable arguments.

The first arguments value is given by the SpecStart element at the beginning of the specification and is
applied to each following fragment until an included specification starts. At that point the included
fragments get the "parent" arguments overriden with their own arguments values, until the included specification
ends.

  Let's see a few examples                                                                             """^
                                                                                                       p^
  "The arguments on a simple specification must be propagated to each fragment"                        ! simple().e1^
  "The arguments on a parent specification must be propagated to a nested one"                         ! nested().e1^
  "The arguments of the nested specification must override the parent ones"                            ! nested().e2^
                                                                                                       p^
  "The SpecsArguments monoid must respect the Monoid laws"                                             !
    SpecsArgumentsMonoid[Fragment].isMonoid                                                            ^
                                                                                                       end

  case class simple() {
    val parent = xonly ^ "s1".title ^ "t1" ^ "e1" ! success
    def e1 = xonlyArgs(parent) must_== List(true, true, true, true)
  }
  case class nested() {
    val child1 = sequential ^ "c1".title ^ "t2"
    val nested1 = simple().parent ^ include(child1)

    def e1 = xonlyArgs(nested1) must_== List(true, true, true, true, true, true)
    def e2 = sequentialArgs(nested1) must_== List(false, false, false, true, true, true)
  }

  def spec(fs: Fragments) = new Specification { def is = fs }
  def argumentsList(fs: Fragments) = foldAll(spec(fs).content.fragments).toList
  def arguments(fs: Fragments) = argumentsList(fs).map(_.toString)
  def xonlyArgs(fs: Fragments) = argumentsList(fs).map(_.xonly)
  def sequentialArgs(fs: Fragments) = argumentsList(fs).map(_.sequential)

  implicit val arbitrarySpecsArguments: Arbitrary[SpecsArguments[Fragment]] = Arbitrary {
    for (f <- arbitrary[Fragment]) yield f
  }
                                                                                          
}