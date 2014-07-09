package org.specs2
package specification
package core

import org.specs2.data.Processes
import text.Trim._
//import scalaz._, Scalaz._

class FragmentsSpec extends Specification with Tables { def is = s2"""
 Fragments can be compacted    $a1
"""
  val factory = fragmentFactory; import factory._

  def a1 = {
    def t(s: String) = text(s)
    def e(s: String) = example(s, success)
    def toFragment(s: String) =
      if (s.trim.startsWith("t")) t(s.trim.drop(1).toString.trimEnclosing("(", ")"))
      else e(s.trim.drop(1).toString.trimEnclosing("(", ")"))

    "Fragments"                  | "result"                            |>
    "t(a1), t(a2)"               ! Seq("a1a2")                         |
    "t(a1), t(a2), e(e1)"        ! Seq("a1a2", "e1")                   |
    "t(a1), t(a2)"               ! Seq("a1a2")                         |
    "t(a1), e(e1), t(a2)"        ! Seq("a1", "e1", "a2")               |
    "e(e1), t(a1), t(a2)"        ! Seq("e1", "a1a2")                   |
    "e(e1), t(a1), t(a2), e(e2)" ! Seq("e1", "a1a2", "e2")             |
    "e(e1), t(a1), e(e2), t(a2)" ! Seq("e1", "a1", "e2", "a2")         |
    { (fs, r) => Fragments(fs.split(",").map(toFragment):_*).compact.fragments.map(_.description.show).toList === r }
  }
}
