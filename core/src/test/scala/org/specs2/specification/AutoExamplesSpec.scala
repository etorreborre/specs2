package org.specs2
package specification

import matcher._
import _root_.org.specs2.mutable.{Specification => Spec}

class AutoExamplesSpec extends Specification with DataTables { def is = s2"""

 The trimCode function should
   remove formatting fragments                                                               $e1
   remove backticks
     with no parameter list                                                                  $e2
     with a parameter list - one param                                                       $e3
     with a parameter list - 2 params                                                        $e4

 DataTables can be used as examples directly
   their description must be left empty, since the result contains the full description      $dt1

 Autoexamples can also be used in mutable specifications                                     $m1
                                                                                             """

  def e1 = "code"                     || "result"                  |>
           "success ^"                !! "success"                 |
           "success ^end^"            !! "success"                 |
           "success ^ end"            !! "success"                 |
           "{ success } ^ end"        !! "success"                 | { (code, result) => trimCode(code) must_== result }

  def e2 = trimCode("`method`") must_== "method"

  def e3 = trimCode("`method`(p1)") must_== "method"

  def e4 = trimCode("`method`(p1, p2)") must_== "method"

  def dt1 = firstExampleDescription("text" ^ datatableOk) must be empty

  def m1 = {
    val spec = new Spec with DataTables {
      { 1 must_== 1 }.eg

      { true }.eg

      { success }.eg

      { datatableOk }.eg
    }
    spec.content.examples must have size(4)
  }

  def firstExampleDescription(fs: Fragments) =
    fs.fragments.collect { case e: Example => e }.head.desc.toString

  def datatableOk =
    "a"   | "b" | "c" |>
     2    !  2  !  4  |
     1    !  1  !  2  | { (a, b, c) =>  a + b must_== c }

}