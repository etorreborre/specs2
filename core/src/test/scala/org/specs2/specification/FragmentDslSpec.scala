package org.specs2.specification

import org.specs2.Specification
import org.specs2.specification.dsl.FragmentsDsl
import FragmentsDsl._
import org.specs2.specification.core.Env
import org.specs2.specification.dsl.FragmentsDsl
import org.specs2.specification.dsl.FragmentsDsl
import org.specs2.specification.dsl.FragmentsDsl
import org.specs2.specification.dsl.FragmentsDsl

class FragmentDslSpec extends Specification { def is = s2"""

 The caret ^ operator can be used to join fragments and build a Fragments object
   f1 ^ f2         $a1
   f1 ^ (f2 ^ f3)  $a2
   (f1 ^ f2) ^ f3  $a3

 with simple strings

   "s" ^ f2         $b4
   f1 ^ "s"         $b5
   "s" ^ (f2 ^ f3)  $b6
   (f1 ^ f2) ^ "s"  $b7

 when using arguments, this creates a SpecStructure object

   args ^ "s"       $c1
   args ^ f1        $c2
   args ^ (f1 ^ f2) $c3

 when using a header, this creates a SpecStructure object

   header ^ "s"        $d1
   header ^ f1         $d2
   header ^ (f1 ^ f2)  $d3

 when using both a header and arguments, this creates a SpecStructure object

   args ^ header ^ "s"         $e1
   header ^ args ^ "s"         $e2
   args ^ header ^ f1          $e3
   header ^ args ^ f1          $e4
   args ^ header ^ (f1 ^ f2)   $e5
   header ^ args ^ (f1 ^ f2)   $e6

 The ! operator can be used to create examples
   "description" ! result                $g1
   "description" ! (s: String) => result $g2
   "description" ! (e: Env) => result    $g3

"""

  def a1 = (f1 ^ f2).fragments must haveSize(2)
  def a2 = (f1 ^ (f2 ^ f3)).fragments must haveSize(3)
  def a3 = (f1 ^ f2 ^ f3).fragments must haveSize(3)

  def b4 = ("s" ^ f2).fragments must haveSize(2)
  def b5 = (f1 ^ "s").fragments must haveSize(2)
  def b6 = ("s" ^ (f2 ^ f3)).fragments must haveSize(3)
  def b7 = (f1 ^ f2 ^ "s").fragments must haveSize(3)

  def c1 = (appendToArguments(xonly) ^ "s").fragments.fragments must haveSize(1)
  def c2 = (xonly ^ f1).fragments.fragments must haveSize(1)
  def c3 = (xonly ^ (f1 ^ f2)).fragments.fragments must haveSize(2)

  def d1 = (header ^ "s").fragments.fragments must haveSize(1)
  def d2 = (header ^ f1).fragments.fragments must haveSize(1)
  def d3 = (header ^ (f1 ^ f2)).fragments.fragments must haveSize(2)

  def e1 = (xonly ^ header ^ "s"      ).fragments.fragments must haveSize(1)
  def e2 = (header ^ xonly ^ "s"      ).fragments.fragments must haveSize(1)
  def e3 = (xonly ^ header ^ f1       ).fragments.fragments must haveSize(1)
  def e4 = (header ^ xonly ^ f1       ).fragments.fragments must haveSize(1)
  def e5 = (xonly ^ header ^ (f1 ^ f2)).fragments.fragments must haveSize(2)
  def e6 = (header ^ xonly ^ (f1 ^ f2)).fragments.fragments must haveSize(2)

  def g1 = ((FragmentsDsl.bangExample("text") ! ok) ^ f1).fragments must haveSize(2)
  def g2 = ((FragmentsDsl.bangExample("text") ! ((s: String) => ok)) ^ f1).fragments must haveSize(2)
  def g3 = ((FragmentsDsl.bangExample("text") ! ((e: Env) => ok)) ^ f1).fragments must haveSize(2)


  val (f1, f2, f3) = (text("t1"), text("t2"), text("t3"))
  val header = FragmentsDsl.title("t").title
}
