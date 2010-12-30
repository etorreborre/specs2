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
 Each Specification provides arguments which can be used to influence its execution and reporting.
 However the scope of the these arguments is restricted to the Fragments belonging to that Specification only.
 Knowing that it is possible to include Specifications into one another, it is necessary to be able to compute
 which arguments are applicable to which fragment. For example, if `xonly` is defined as an argument, then only
 failing examples will be reported.

 So the responsability of the SpecsArguments class is to map a Seq of fragments to their applicable arguments.

 The first `Arguments` value is given by the `SpecStart` element at the beginning of the Specification and is
 applied to each following fragment until an included specification starts. At that point, the included
 fragments get the "parent" arguments overriden with their own arguments values, until the included specification
 ends.

  Let's see a few examples                                                                             """^
                                                                                                       p^
  "On a simple specification the arguments must be propagated to each fragment"                        ! simple().e1^
  "The arguments of a parent specification must be propagated to a nested one"                         ! nested().e1^
  "The arguments of the nested specification must override the parent ones"                            ! nested().e2^
                                                                                                       p^
  "The SpecsArguments monoid must respect the Monoid laws"                                             !
    SpecsArgumentsMonoid[Fragment].isMonoid                                                            ^
                                                                                                       end

  case class simple() {
    val parent = spec(xonly ^ "s1".title ^ "t1" ^ "e1" ! success)
    def e1 = xonlyArgs(parent) must_== List(true, true, true, true)
  }
  case class nested() {
    val child1 = spec(sequential ^ "c1".title ^ "t2")
    val nested1 = spec(simple().parent ^ include(child1))

    def e1 = xonlyArgs(nested1) must_== List(true, true, true, true, true, true, true)
    def e2 = sequentialArgs(nested1) must_== List(false, false, false, true, true, true, false)
  }

  def spec(fs: Fragments) = new Specification { def is = fs }.content
  def argumentsList(fs: Fragments) = foldAll(fs.fragments).toList
  def arguments(fs: Fragments) = argumentsList(fs).map(_.toString)
  def xonlyArgs(fs: Fragments) = argumentsList(fs).map(_.xonly)
  def sequentialArgs(fs: Fragments) = argumentsList(fs).map(_.sequential)

  implicit val arbitrarySpecsArguments: Arbitrary[SpecsArguments[Fragment]] = Arbitrary {
    for (f <- arbitrary[Fragment]) yield f
  }
                                                                                          
}