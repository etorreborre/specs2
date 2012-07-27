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
