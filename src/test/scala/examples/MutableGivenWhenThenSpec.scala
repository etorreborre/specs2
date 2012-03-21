package examples

import org.specs2.mutable.Specification

/**
* This specification shows how to use the Given/When/Then style for a unit specifications.
*/
class MutableGivenWhenThenSpec extends Specification { noindent

  "A given-when-then example for a calculator".br

    "Given the following number: ${1}" << { s: String =>
      a = s.toInt
    }
    "And a second number: ${2}" << { s: String =>
      b = s.toInt
    }
    "When I use this operator: ${+}" << { s: String =>
      result = Operation(a, b, s).calculate
    }
    "Then I should get: ${3}" << { s: String =>
      result === s.toInt
    }
    "And it should be > ${0}" << { s: String =>
      result must be_>(s.toInt)
    }

  var a, b, result: Int = 0

  case class Operation(n1: Int, n2: Int, operator: String) {
    def calculate: Int = if (operator == "+") n1 + n2 else n1 * n2
  }
}

