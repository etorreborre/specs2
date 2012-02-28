package user.specification

import org.specs2.mutable
import org.specs2.specification.AllExpectations

class AllExpectationsSpecification extends mutable.Specification with AllExpectations {
  "In this example all the expectations are evaluated" >> {
    1 === 2
    1 === 3
    1 === 1
  }
  "There is no collision with this example" >> {
    10 === 11
    12 === 31
    13 === 13
  }
}
