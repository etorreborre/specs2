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
  The Specs Arguments class is a monoid which, when folding over fragments, keeps the 
  arguments that are declared by the latest SpecStart. When a new SpecStart is 'appended'
  then the current arguments change.                                                                                         
                                                                                          """^
  "Two specs one after the other"                                                         ! ex1^
  "One spec included into the other"                                                      ! ex2^
                                                                                          p^
  "The SpecsArguments monoid must respect the Monoid laws"                                !
    SpecsArgumentsMonoid[Fragment].isMonoid                                               ^
                                                                                          end

  val s1: SpecsArguments[Fragment] = SpecStart("spec1", args(ex="s1"))
  val s2: SpecsArguments[Fragment] = SpecStart("spec2", args(ex="s2"))
  val e1: SpecsArguments[Fragment] = SpecEnd("spec1")
  val e2: SpecsArguments[Fragment] = SpecEnd("spec2")
  val t1: SpecsArguments[Fragment] = Text("text1")
  val t2: SpecsArguments[Fragment] = Text("text2")
  val t3: SpecsArguments[Fragment] = Text("text3")

  def ex1 = (s1 |+| t1 |+| s2 |+| t2).toList must_== List(args(ex = "s1"), args(ex = "s1"), args(ex = "s2"), args(ex = "s2"))
  def ex2 = (s1 |+| t1 |+| s2 |+| t2 |+| e2 |+| t3 |+| e1).toList must_==
           List(args(ex = "s1"),
                args(ex = "s1"),
                  args(ex = "s2"),
                  args(ex = "s2"),
                  args(ex = "s2"),
                args(ex = "s1"),
                args(ex = "s1"))

  implicit val arbitrarySpecsArguments: Arbitrary[SpecsArguments[Fragment]] = Arbitrary {
    for (f <- arbitrary[Fragment]) yield f
  }
                                                                                          
}