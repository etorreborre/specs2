package org.specs2
package examples

import specification.{Before, BeforeEach, BeforeExample}

/**
 * This specification shows various ways to setup contexts for examples.
 *
 * For more details @see org.specs2.guide.SpecStructure
 */
class DefineContextsSpec extends Specification {

  /**
   * This specification uses the `BeforeExample` trait to execute some code before each example
   * In that case you need to define the `before` method in the specification
   */
  class BeforeExampleSpec extends Specification with BeforeExample { def is =
    "This is a list of examples"                                     ^
      "example1"                                                     ! success^
      "example2"                                                     ! success^
                                                                     end
    def before = println("clean up before each example")
  }

  /**
   * This mutable specification also uses the `BeforeExample` trait
   */
  class BeforeExampleMutableSpec extends mutable.Specification with BeforeExample {
    "This is a list of examples" >> {
      "example1"                 >> success
      "example2"                 >> success
    }

    def before = println("clean up before each example")
  }

  /**
   * This specification uses a context object extending the `BeforeEach` trait to execute some code before each example
   * In that case you need to define an object extending `BeforeEach`, implement the `before` method inside this object
   * and apply that object to all the specification fragments
   */
  class BeforeEachSpec extends Specification { def is = clean(spec)
    def spec =
    "This is a list of examples"                                     ^
      "example1"                                                     ! success^
      "example2"                                                     ! success^
                                                                     end

    object clean extends BeforeEach {
      def before = println("clean up before each example")
    }
  }

  /**
   * This specification uses a context class extending the `Before`.
   * It is similar to the previous case but allows to use "fresh" variables in each example
   */
  class BeforeSpec extends Specification { def is =
    "This is a list of examples"                                     ^
      "example1"                                                     ! clean().e1^
      "example2"                                                     ! clean().e2^
                                                                     end

    case class clean() extends Before {
      val aNewSystem = "a fresh value"
      def before = println("clean up before each example")

      def e1 = aNewSystem must_== "a fresh value"
      def e2 = aNewSystem must_== "a fresh value"
    }
  }

  def println(s: String) = s // change this definition to see messages in the console

  def is = new BeforeSpec ^ new BeforeExampleMutableSpec ^ new BeforeEachSpec ^ new BeforeExampleSpec
}