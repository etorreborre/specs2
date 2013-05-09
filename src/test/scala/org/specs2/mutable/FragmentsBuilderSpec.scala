package org.specs2
package mutable

import text.Trim._
import specification.{AcceptanceCreationPath, MutableCreationPath, AllExpectations}
import scalaz.Scalaz._
import execute.Result

class FragmentsBuilderSpec extends Specification with AllExpectations {

  "Creating fragments must maintain a tree showing all the creation paths for each block" >> {
    spec1.content
    spec1.blocksTree.toTree.map(b => (b._1, b._2.toString)).drawTree.trimNewLines ====
      """
      |(0,"Text(root)")
      ||
      |+- (0,"Text(a)")
      ||  |
      ||  +- (0,"Text(b)")
      ||  |  |
      ||  |  `- (0,"Example(c)")
      ||  |
      ||  +- (1,"Backtab(1)")
      ||  |
      ||  `- (2,"Example(d)")
      ||
      |+- (1,"Backtab(1)")
      ||
      |+- (2,"Text(e)")
      ||  |
      ||  `- (0,"Example(f)")
      ||
      |`- (3,"Backtab(1)")
      """.stripMargin.trimNewLines
  }
  "Examples must be created with their 'blockCreationPath'" >> {
    "for a mutable specification" >> {
      spec1.content.examples(0).creationPath ==== Some(MutableCreationPath(Seq(0, 0, 0, 0)))
      spec1.content.examples(1).creationPath ==== Some(MutableCreationPath(Seq(0, 0, 2)))
      spec1.content.examples(2).creationPath ==== Some(MutableCreationPath(Seq(0, 2, 0)))
    }
    "for an acceptance specification" >> {
      spec2.content.examples(0).creationPath ==== Some(AcceptanceCreationPath(Seq(3)))
      spec2.content.examples(1).creationPath ==== Some(AcceptanceCreationPath(Seq(4)))
      spec2.content.examples(2).creationPath ==== Some(AcceptanceCreationPath(Seq(6)))
    }
  }
  "It is possible to collect all the fragments which are created on a given 'path'" >> {
    val example = spec1.content.examples(2)
    spec1.fragmentsTo(example) must contain(be_==(example))
  }

  "Fragments creation with Unit" >> {
    "a block returning Unit is interpreted as a block of fragments when the examplesBlock method is used" >> {
      val s = new Specification {
        "this system has 3 examples" >> { examplesBlock((1 to 3) foreach { i => "example "+i >> ok }) }
      }
     s.content.examples must have size(3)
    }
    "a block returning Unit is interpreted as a block of expectations when the Result.unit method is used" >> {
      val s = new Specification {
        "this system has 1 example" >> { "example" in { Result.unit((1 to 3) foreach { i => i ==== i }) } }
      }
      s.content.examples must have size(1)
    }
  }

  lazy val spec1 = new FragmentsBuilderExample1
  lazy val spec2 = new FragmentsBuilderExample2
}

class FragmentsBuilderExample1 extends Specification {
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

class FragmentsBuilderExample2 extends org.specs2.Specification { def is = isolated ^ sequential ^
  "a"   ^
  "b"   ^
    "c" ! ok ^
    "d" ! ok ^
  "e"   ^
  "f"   ! ok
}
