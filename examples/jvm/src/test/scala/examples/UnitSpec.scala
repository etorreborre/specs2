package examples

import org.specs2.*

/**
 * This specification shows how to use the mutable.Specification trait to create a unit Specification
 *
 * It shows most of the features of mutable specifications:
 *
 *  - how to create a title
 *  - how to set arguments
 *  - how to create Steps
 *  - how to create examples
 *  - how to use traits to provide vals to each example and set the context
 *  - how create references to other specifications or include them
 *
 */
class UnitSpec extends mutable.Specification:

  // A title can be added at the beginning of the specification
  "MutableSpec".title

  // arguments are simply declared at the beginning of the specification if needed
  args(xonly=true)

  // a step to execute before the specification must be declared first
  step {
    // setup your data or initialize your database here
    success
  }

  "'Hello world'  should" >> {
    "contain 11 characters" >> {
      "Hello world" `must` haveSize(11)
    }
    "start with 'Hello'" >> {
      "Hello world" `must` startWith("Hello")
    }
    /**
     * a failing example will stop right away, without having to "chain" expectations
     */
    "with 'world'" >> {
      // Expectations are throwing exception by default so un-commenting this line will
      // stop the execution right away with a Failure
      // "Hello world" must startWith("Hi")

      "Hello world" `must` endWith("world")
    }
  }
  // you can add references to other specifications
  "how" ~ (new IncludedSpecification)

  // a step to execute after the specification must be declared at the end
  step {
    // close the database here
    success
  }

  class IncludedSpecification extends Specification { def is = "introduction" ^ "example" ! success }
