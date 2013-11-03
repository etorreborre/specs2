package org.specs2
package specification

import matcher._

class FragmentsBuilderIntegrationSpec extends script.Specification with ResultMatchers with TerminationMatchers with Groups {  def is = s2"""

 In a Specification, the `contents` variable stores an instance of the Fragments class,

 Blocks
 ======

 With mutable specs it is desirable to create examples or expectations by looping with a foreach loop
  + mixing in the trait ExamplesBlock allows to create examples
  + mixing in the trait ExpectationsBlock allows to create expectations

 Links
 =====

  + A specification can reference itself without creating a loop

                                                                                                          """
  "blocks" - new group {
    eg := {
      new org.specs2.mutable.Specification with org.specs2.mutable.ExamplesBlock {
        "this should compile" >> { (1 to 3).foreach { i => s"test $i" in ok } }
      }
      ok
    }

    eg := {
      new org.specs2.mutable.Specification with org.specs2.mutable.ExpectationsBlock {
        "this should compile" >> { (1 to 3).foreach { i => s"test $i" must startWith("test") } }
      }
      ok
    }
  }

  "links" - new group with specifications {
    // this terminates with a StackOverflow exception
    eg := selfReferencing.content must terminate
  }

  trait specifications {
    lazy val selfReferencing: Specification = new Specification { def is = "e1" ^ see(selfReferencing) }
  }
}