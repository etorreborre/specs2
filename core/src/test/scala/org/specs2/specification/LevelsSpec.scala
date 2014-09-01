package org.specs2
package specification

import scalaz.Tree
import core._
import Fragment._
import process._
import Levels._

class LevelsSpec extends Spec { def is = s2"""

 The Levels class is used to compute the 'level' of Fragments in a list of Fragments.
 This is used with mutable specifications to be able to display a tree of examples (Acceptance specification just list their examples on the same level)

  A tree of fragments can be created from the leveled blocks
    for "t1" >> { "ex1" in ok ;  "ex2" in ok }                                           ${tree().e1}
    for "t1" >> { "t2" >> { "ex1" in ok ;  "ex2" in ok } }                               ${tree().e2}
                                                                                         """

  trait spec extends org.specs2.mutable.Specification
  case class tree() {
    def e1 = treeMap(new spec { "t1" >> { "e1" in ok; "e2" in ok } }.is.fragments)(mapper) must beDrawnAs(
      "Fragment(root)",
      "|",
      "`- Fragment(t1)",
      "   |",
      "   +- Fragment(e1)",
      "   |",
      "   `- Fragment(e2)")

    def e2 = treeMap(new spec { "t1" >> { "e1" in ok }; "t2" >> { "e2" in ok } }.is.fragments)(mapper) must beDrawnAs(
      "Fragment(root)",
      "|",
      "+- Fragment(t1)",
      "|  |",
      "|  `- Fragment(e1)",
      "|",
      "`- Fragment(t2)",
      "   |",
      "   `- Fragment(e2)")

    // use this mapper to keep only text and examples
    val mapper = (f: Fragment) => {
      f match {
        case Fragment(Text(t), _, _) if t.trim.isEmpty => None
        case Fragment(Text(t), e, l)  => Some(Fragment(Text(t.trim), e, l))
        case other                       => None
      }
    }
    def beDrawnAs(lines: String*) = be_==(lines.mkString("", "\n", "\n")) ^^ {
      tree: Option[Tree[Fragment]] => tree.map(_.drawTree).getOrElse("no tree!")
    }
  }

}
