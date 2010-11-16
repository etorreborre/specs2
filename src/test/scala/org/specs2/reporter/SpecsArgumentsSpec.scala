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

class SpecsArgumentsSpec extends SpecificationWithJUnit with ScalazMatchers
with ArbitraryFragments { def is =
                                                                                          """
  The Specs Arguments class is a monoid which, when folding over fragments, keeps the 
  arguments that are declared by the latest SpecStart. When a new SpecStart is 'appended'
  then the current arguments change.                                                                                         
                                                                                          """^
  "Following a SpecStart with some Text must not change the arguments"                    ! e1^
  "Following a Text fragment with a SpecStart must change the arguments"                  ! e2^
  "Following a Text fragment with a Text Fragment must not change the arguments"          ! e3^
  "Following a SpecStart with another SpecStart must change the arguments"                ! e4^
                                                                                          p^
  "The SpecsArguments monoid must respect the Monoid laws"                                !
    SpecsArgumentsMonoid.isMonoid                                                         ^
                                                                                          end

  val s1: SpecsArguments = SpecStart("start1", args(ex="s1"))
  val s2: SpecsArguments = SpecStart("start2", args(ex="s2"))
  val t1: SpecsArguments = Text("text1")
  val t2: SpecsArguments = Text("text2")
  
  def e1 = (s1 |+| t2).args must_== args(ex = "s1")
  def e2 = (t1 |+| s2).args must_== args(ex = "s2")
  def e3 = (t1 |+| t2).args must_== Arguments()
  def e4 = (s1 |+| s2).args must_== args(ex = "s2")
                                                                                          
  implicit val arbitrarySpecsArguments: Arbitrary[SpecsArguments] = Arbitrary {
    for (f <- arbitrary[Fragment]) yield f
  }
                                                                                          
}