package org.specs2
package specification

import matcher._
import _root_.org.specs2.mutable.{Specification => Spec}
import org.specs2.specification.core.{Fragment, Fragments}
import org.specs2.specification.create.AutoExamples
import org.specs2.specification.dsl.FragmentsDsl

class AutoExamplesSpec extends org.specs2.Spec with DataTables with AutoExamples with FragmentsDsl { def is = s2"""

 The trimExpression function should
   remove backticks
     with no parameter list                                                                  $e1
     with a parameter list - one param                                                       $e2
     with a parameter list - 2 params                                                        $e3

 DataTables can be used as examples directly                                                 $dt1

 Autoexamples can also be used in mutable specifications                                     $m1
                                                                                             """

  def e1 = trimExpression("`method`") must_== "method"
  def e2 = trimExpression("`method`(p1)") must_== "method"
  def e3 = trimExpression("`method`(p1, p2)") must_== "method"

  def dt1 = firstExampleDescription("text" ^ datatableOk) must contain(
  """|"a"  | "b" | "c" |>
     |2    ! 2   ! 4   |
     |1    ! 1   ! 2   | { (a, b, c) =>  a + b must_== c }""".stripMargin)

  def m1 = {
    val spec = new Spec with DataTables {
      eg { 1 must_== 1 }

      eg { true }

      eg { success }

      eg {
        "a"  | "b" | "c" |>
        2    ! 2   ! 4   |
        1    ! 1   ! 2   | { (a, b, c) =>  a + b must_== c }
      }
    }
    spec.is.fragments.fragments.filter(Fragment.isExample) must haveSize(4)
  }

  def firstExampleDescription(fs: Fragments) =
    fs.fragments.filter(Fragment.isExample).head.description.show

  def datatableOk = eg {
    "a"  | "b" | "c" |>
    2    ! 2   ! 4   |
    1    ! 1   ! 2   | { (a, b, c) =>  a + b must_== c }
  }

}