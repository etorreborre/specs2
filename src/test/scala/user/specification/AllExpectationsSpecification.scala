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
  "It is possible to short-circuit the rest of the evaluation with 'orThrow'" >> {
    10 === 10
   (51 === 52).orThrow
    13 === 14
  }
  "It is possible to short-circuit the rest of the evaluation with 'orSkip'" >> {
    10 === 10
    (51 === 52).orSkip
    15 === 16
  }
}
