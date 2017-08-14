package org.specs2
package specification
package script

import main.Arguments
import runner.TextRunner
import specification.create.{S2StringContext, FragmentsFactory}
import matcher.MatchersImplicits._

class SpecificationSpec extends script.Spec with Grouped { def is = s2"""

 Examples creation
 ================

 A specification can just be written with some text and bullet points for the examples
 + one group, one example
 + when there is a new title then a new `Group` should be used

 Tags
 ====

 + each example must be tagged with its corresponding group index and example index
 + each group of examples must be tagged as a section


"""

  "examples creation" - new g1 with sampleGroups {
    e1 := {
      val result = run("""This is a specification with 2 examples
                            + ex1
                            + ex2""", g1ok)

      forall(Seq("\\+ ex1", "\\+ ex2")) { ex => result must containMatch(ex) }
    }
    e2 := {
      val result = run("""a first group
                            + ex1
                            + ex2
                          # Title
                          and a second one
                            + ex1
                            + ex2
                       """)

      forall(Seq("g1.e1", "g1.e2", "g2.e1", "g2.e2")) { ex => result must containMatch(ex) }
    }
  }

  "tags" - new g2 with sampleGroups {
    e1 := {
      val result = run("""This is a specification with 2 examples
                            + ex1
                            + ex2""")(exclude("g2.e2"))

      result must not(containMatch("\\+ ex2"))
    }

    e2 := {
      val text = """This is a specification with 2 groups
                            + ex1
                            + ex2
                    # title
                          second one
                            + fx1
                            + fx2
                 """
      val result = run(text, g1ok, g2ok)(exclude("g1"))
      (result must not(containMatch("\\+ ex2"))) and
      (result must containMatch("\\+ fx1"))
    }
  }

  def run(text1: String, groups: ExamplesGroup*)(implicit arguments: Arguments = Arguments()): Seq[String] = {
    val spec = new script.Specification with Grouped { outer =>
      def is = arguments ^ sequential ^   nocolor ^ s2"""$text1"""
      (0 until groups.size) foreach { i =>
        (0 until 22).foreach(j => outer.createExamplesGroup(i).createExample(j) := groups(i).createExample(j))
      }
    }
    TextRunner.run(spec).messages
  }


  trait sampleGroups extends Groups with S2StringContext with FragmentsFactory {
    val (g1ok, g2ok) = (new g1 { e1 := ok; e2 := ok }, new g2 { e1 := ok; e2 := ok })
  }

}
