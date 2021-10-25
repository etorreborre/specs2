package org.specs2
package specification

import org.specs2.fp._
import core._
import Fragment._
import process._
import Levels._
import org.specs2.concurrent.ExecutionEnv
import org.specs2.matcher.MatchResult
import org.specs2.specification.dsl.AcceptanceDsl

class LevelsSpec(ee: ExecutionEnv) extends Spec { def is = s2"""

 The Levels class is used to compute the 'level' of Fragments in a list of Fragments.
 This is used with mutable specifications to be able to display a tree of examples (Acceptance specification just list their examples on the same level)

  A tree of fragments can be created from the leveled blocks
    for mutable spec:"e1" in ok; "e2" in ok                                              ${tree().e0}
    for mutable spec:"t1" >> { "ex1" in ok ;  "ex2" in ok }                              ${tree().e1}
    for mutable spec:"t1" >> { "t2" >> { "ex1" in ok ;  "ex2" in ok } }                  ${tree().e2}
    for mutable spec with three nested levels and 'for loop'                             ${tree().e3}
    for non mutable spec                                                                 ${tree().e4}
    for non mutable spec with fragments                                                  ${tree().e5}
                                                                                         """

  trait mutableSpec extends org.specs2.mutable.Specification
  trait spec extends org.specs2.Specification
  case class tree() extends AcceptanceDsl {
    def e0: MatchResult[Option[Tree[Fragment]]] = treeMap(new mutableSpec { "e1" in ok; "e2" in ok }.is.fragments)(mapper)(ee) must beDrawnAs(
      "Fragment(root)",
      "|",
      "+- Fragment(e1)",
      "|",
      "`- Fragment(e2)")

    def e1: MatchResult[Option[Tree[Fragment]]] = treeMap(new mutableSpec { "t1" >> { "e1" in ok; "e2" >> ok } }.is.fragments)(mapper)(ee) must beDrawnAs(
      "Fragment(root)",
      "|",
      "`- Fragment(t1)",
      "   |",
      "   +- Fragment(e1)",
      "   |",
      "   `- Fragment(e2)")

    def e2: MatchResult[Option[Tree[Fragment]]] = treeMap(new mutableSpec { "t1" >> { "e1" in ok }; "t2" >> { "e2" in ok } }.is.fragments)(mapper)(ee) must beDrawnAs(
      "Fragment(root)",
      "|",
      "+- Fragment(t1)",
      "|  |",
      "|  `- Fragment(e1)",
      "|",
      "`- Fragment(t2)",
      "   |",
      "   `- Fragment(e2)")

    def e3: MatchResult[Option[Tree[Fragment]]] = treeMap(new mutableSpec {
      "t1" >> {
        br
        "t2" >> {
          ok
          Fragment.foreach(1 to 3) { i =>
            "e" + i in ok
          }
          "t3" >> {
            "e4" in ok
          }

          "e5" in ok
        }
        "e6" in ok
      }
    }.is.fragments)(mapper)(ee) must beDrawnAs(
      "Fragment(root)",
      "|",
      "`- Fragment(t1)",
      "   |",
      "   +- Fragment(t2)",
      "   |  |",
      "   |  +- Fragment(e1)",
      "   |  |",
      "   |  +- Fragment(e2)",
      "   |  |",
      "   |  +- Fragment(e3)",
      "   |  |",
      "   |  +- Fragment(t3)",
      "   |  |  |",
      "   |  |  `- Fragment(e4)",
      "   |  |",
      "   |  `- Fragment(e5)",
      "   |",
      "   `- Fragment(e6)")

    def e4: MatchResult[Option[Tree[Fragment]]] = treeMap(new spec { def is = s2"""
        t11
          e11 $ok
          e22 $ok
          """
    }.is.fragments)(mapper)(ee) must beDrawnAs(
      "Fragment(root)",
      "|",
      "+- Fragment(t11)",
      "|",
      "+- Fragment(e11)",
      "|",
      "`- Fragment(e22)"
    )

    def e5: MatchResult[Option[Tree[Fragment]]] = treeMap(new spec { def is = s2"""
        Explanation Text.

        First starting test line

          example no. one $ok
          example no. two $ok

        Second starting test line
          first set of fragments $fragments1
          second set of fragments $fragments2
          """
    }.is.fragments)(mapper)(ee) must beDrawnAs(
      "Fragment(root)",
      "|",
      "+- Fragment(Explanation Text. First starting test line)",
      "|",
      "+- Fragment(example no. one)",
      "|",
      "+- Fragment(example no. two)",
      "|",
      "+- Fragment(Second starting test line first set of fragments)",
      "|",
      "+- Fragment(example no. one from first set)",
      "|",
      "+- Fragment(example no. two from first set)",
      "|",
      "+- Fragment(second set of fragments)",
      "|",
      "+- Fragment(example no. one from second set)",
      "|",
      "`- Fragment(example no. two from second set)"
    )

    private def fragments1 = fragments("first")
    private def fragments2 = fragments("second")

    private def fragments(description: String) = {
      p^
        s"example no. one from $description set"  ! success ^br^
        s"example no. two from $description set"  ! success ^br
    }

    // use this mapper to keep only text and examples
    private val mapper = (f: Fragment) => {
      f match {
        case Fragment(Text(t), _, _) if t.trim.isEmpty  => None
        case Fragment(Text(t), e, l)                    => Some(Fragment(Text(t.trim.replaceAll("\\s+"," ")), e, l))
        case _                                          => None
      }
    }
    private def beDrawnAs(lines: String*) = be_==(lines.mkString("", "\n", "\n")) ^^ {
      (tree: Option[Tree[Fragment]]) => tree.map(_.drawTree).getOrElse("no tree!")
    }
  }
}
