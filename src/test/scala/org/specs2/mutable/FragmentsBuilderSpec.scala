package org.specs2
package mutable
import text.Trim._
import specification.AllExpectations
import internal.scalaz.Scalaz._

class FragmentsBuilderSpec extends Specification with AllExpectations {

  "Creating fragments must maintain a tree showing all the creation paths for each block" >> {
    spec.blocksTree.toTree.map(b => (b._1, b._2.toString)).drawTree.trimNewLines ====
      """
      |(0, Text(root))
      ||
      |+- (0, Text(a))
      ||  |
      ||  +- (0, Text(b))
      ||  |  |
      ||  |  `- (0, Example(c))
      ||  |
      ||  +- (1, Backtab(1))
      ||  |
      ||  `- (2, Example(d))
      ||
      |+- (1, Backtab(1))
      ||
      |+- (2, Text(e))
      ||  |
      ||  `- (0, Example(f))
      ||
      |`- (3, Backtab(1))
      """.stripMargin.trimNewLines
  }
  "Examples must be created with their 'blockCreationPath'" >> {
    spec.content.examples(0).creationPath ==== Seq(0, 0, 0, 0)
    spec.content.examples(1).creationPath ==== Seq(0, 0, 2)
    spec.content.examples(2).creationPath ==== Seq(0, 2, 0)
  }
  "Fragments creation with Unit" >> {
    "if a block returning Unit is created with '>>', then it is interpreted as a block of fragments" >> {
      val s = new Specification {
        "this system has 3 examples" >> { (1 to 3) foreach { i => "example "+i >> ok } }
      }
     s.content.examples must have size(3)
    }
    "if a block returning Unit is created with 'in', then it is interpreted as a block of expectations and creates an Example" >> {
      val s = new Specification {
        "this system has 1 example" >> { "example" in { (1 to 3) foreach { i => i ==== i } } }
      }
      s.content.examples must have size(1)
    }
  }

  lazy val spec = new Specification {
    "a" >> {
      "b" >> {
        "c" >> ok
      }
      "d" >> ok
    }
    "e" >> {
      "f" >> ok
    }
  }

}
