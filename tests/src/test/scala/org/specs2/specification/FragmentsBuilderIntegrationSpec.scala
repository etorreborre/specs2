package org.specs2
package specification

import matcher._

class FragmentsBuilderIntegrationSpec extends script.Specification with ResultMatchers with TerminationMatchers with Groups {  def is = s2"""

 In a Specification, the `contents` variable stores an instance of the Fragments class,

Links
=====
 + A specification can reference itself without creating a loop

                                                                                                          """
  "links" - new group with specifications {
    // this terminates with a StackOverflow exception
    eg := selfReferencing.content must terminate
  }

  trait specifications {
    lazy val selfReferencing: Specification = new Specification { def is = "e1" ^ see(selfReferencing) }
  }
}