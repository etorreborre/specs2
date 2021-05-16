package examples

import org.specs2.*

class HelloWorldUnitSpec extends mutable.Specification:
  "This is a specification to check the 'Hello world' string".br

  "The 'Hello world' string should" >> {
    "contain 11 characters" >> {
      "contain 11 characters" >> {
         "contain 11 characters" >> {
           "Hello world" must haveSize(11)
         }
      }
    }

    "start with 'Hello'" >> {
      "Hello world" must startWith("Hello")
    }

    "end with 'world'" >> {
      "Hello world" must endWith("world")
    }
  }
