package org.specs2
package guide

object GetAllExpectations extends UserGuidePage { def is = s2"""

The 2 main modes of expectations in $specs2 are:

  - No thrown expectations
  - thrown expectations

They correspond to different style of declaring expectations and you will use one or the other depending on how many expectations you have per example.

The `org.specs2.specification.AllExpectations` trait goes further and gives you the possibility to report _all_ the failures of an Example without stopping at the first failure. This enables a type of specification where it is possible to define lots of expectations inside the body of an example and get a maximum of information on what fails and what passes: ${snippet{
import org.specs2.specification.AllExpectations
import org.specs2.mutable.Specification

class AllExpectationsSpec extends Specification with AllExpectations {
  "In this example all the expectations are evaluated" >> {
    1 === 2  // this fails
    1 === 3  // this also fails
    1 === 1
  }
  "There is no collision with this example" >> {
    10 === 11 // this fails
    12 === 12
    13 === 31 // this also fails
  }
}
}}

The second example above hints at a restriction for this kind of Specification. The failures are accumulated for each example by mutating a shared variable. "Mutable" means that the concurrent execution of examples will be an issue if done blindly. To avoid this, the `AllExpectations` trait overrides the `Specification` arguments to make it ${"isolated" ~/ Execution} (unless it is already `isolated` or `sequential`).

"""

}
