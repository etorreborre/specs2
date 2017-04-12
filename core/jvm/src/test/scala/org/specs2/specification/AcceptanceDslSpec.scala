package org.specs2
package specification

import core.Env
import dsl._
import org.specs2.concurrent.ExecutionEnv

class AcceptanceDslSpec extends Spec with AcceptanceDsl { def is = s2"""

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

 ${step(ee.shutdown)}
"""
  implicit val ee: ExecutionEnv =
    Env().executionEnv

  val factory = fragmentFactory; import factory._

  def a1 = (f1 ^ f2).fragmentsList(ee) must haveSize(2)
  def a2 = (f1 ^ (f2 ^ f3)).fragmentsList(ee) must haveSize(3)
  def a3 = (f1 ^ f2 ^ f3).fragmentsList(ee) must haveSize(3)

  def b4 = ("s" ^ f2).fragmentsList(ee) must haveSize(2)
  def b5 = (f1 ^ "s").fragmentsList(ee) must haveSize(2)
  def b6 = ("s" ^ (f2 ^ f3)).fragmentsList(ee) must haveSize(3)
  def b7 = (f1 ^ f2 ^ "s").fragmentsList(ee) must haveSize(3)

  def c1 = (appendToArguments(xonly) ^ "s").fragments.fragmentsList(ee) must haveSize(1)
  def c2 = (xonly ^ f1).fragments.fragmentsList(ee) must haveSize(1)
  def c3 = (xonly ^ (f1 ^ f2)).fragments.fragmentsList(ee) must haveSize(2)

  def d1 = (header ^ "s").fragments.fragmentsList(ee) must haveSize(1)
  def d2 = (header ^ f1).fragments.fragmentsList(ee) must haveSize(1)
  def d3 = (header ^ (f1 ^ f2)).fragments.fragmentsList(ee) must haveSize(2)

  def e1 = (xonly ^ header ^ "s"      ).fragments.fragmentsList(ee) must haveSize(1)
  def e2 = (header ^ xonly ^ "s"      ).fragments.fragmentsList(ee) must haveSize(1)
  def e3 = (xonly ^ header ^ f1       ).fragments.fragmentsList(ee) must haveSize(1)
  def e4 = (header ^ xonly ^ f1       ).fragments.fragmentsList(ee) must haveSize(1)
  def e5 = (xonly ^ header ^ (f1 ^ f2)).fragments.fragmentsList(ee) must haveSize(2)
  def e6 = (header ^ xonly ^ (f1 ^ f2)).fragments.fragmentsList(ee) must haveSize(2)

  def g1 = ((bangExample("text") ! ok) ^ f1).fragmentsList(ee) must haveSize(2)
  def g2 = ((bangExample("text") ! ((s: String) => ok)) ^ f1).fragmentsList(ee) must haveSize(2)
  def g3 = ((bangExample("text") ! ((e: Env) => ok)) ^ f1).fragmentsList(ee) must haveSize(2)


  val (f1, f2, f3) = (text("t1"), text("t2"), text("t3"))
  val header = title("t").title
}
