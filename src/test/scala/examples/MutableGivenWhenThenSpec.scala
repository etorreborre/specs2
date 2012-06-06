package examples

import org.specs2.mutable.Specification
import org.specs2.specification.{Then, When, RegexStep, Given}
import org.specs2.execute.Result

/**
* This specification shows how to use the Given/When/Then style for a unit specifications.
*/
class MutableGivenWhenThenSpec extends Specification { noindent

  "A given-when-then example for a calculator" >> {
    var a, b, result: Int = 0

    "Given the following number: ${1}" << { (s: String) =>
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
  }

  "A given-when-then example for a calculator - with regular expressions" >> {
    var a, b, result: Int = 0

    // Reading in the text with a regular expression
    "Given the following number: 1" << readAs(".*(\\d).*") { s: String =>
      a = s.toInt
    }
    // Reading in the text with a regular expression with only the capturing group
    // and extracting a variable number of elements
    "And a second number: 2" << groupAs("\\d") { s: Seq[String] =>
      b = s.head.toInt
    }
    // Reading in the text with a regular expression with only the capturing group
    // and extracting a fix number of elements
    "When I use this operator: +" << groupAs("[\\+\\-]") { s: String =>
      result = Operation(a, b, s).calculate
    }
    "Then I should get: 3" << groupAs("\\d") { s: String =>
      result === s.toInt
    }
    "And it should be > 0" << groupAs("\\d") { s: String =>
      result must be_>(s.toInt)
    }
  }

  case class Operation(n1: Int, n2: Int, operator: String) {
    def calculate: Int = if (operator == "+") n1 + n2 else n1 * n2
  }

}

