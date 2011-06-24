package examples

import org.specs2._
import specification.{Before, BeforeExample}

/**
 * This specification shows various ways to setup contexts for examples.
 *
 * For more details @see org.specs2.guide.SpecStructure
 */
class DefineContextsSpec extends Specification {

  /**
   * This specification uses a context class extending the `Before` trait.
   * It is also creating "fresh" variables for each example
   */
  class BeforeSpec extends Specification { def is =
    "This is a list of examples"                                     ^
      "example1"                                                     ! clean().e1^
      "example2"                                                     ! clean().e2^
                                                                     end

    case class clean() extends Before {
      val aNewSystem = "a fresh value"
      def before = println("clean up before each example")

      def e1 = this { aNewSystem must_== "a fresh value" }
      def e2 = this { aNewSystem must_== "a fresh value" }
    }
  }

  /**
   * Same thing as above for a mutable specification
   */
  class BeforeMutableSpec extends mutable.Specification {
    "This is a list of examples" >> {
      "example1" >> new clean {
        aNewSystem must_== "a fresh value"
      }
      "example2" >> new clean {
        aNewSystem must_== "a fresh value"
      }
    }

    /** here we need a trait extending mutable.Before because the example body will be executed as a "delayed init"  section*/
    trait clean extends mutable.Before {
      lazy val aNewSystem = "a fresh value"
      def before = println("clean up before each example")
    }
  }

  /**
   * This specification uses the `BeforeExample` trait to execute some code before each example
   * by simply defining a `before` method
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

  def println(s: String) = s // change this definition to see messages in the console

  def is = new BeforeSpec ^ new BeforeMutableSpec ^ new BeforeExampleMutableSpec ^ new BeforeExampleSpec
}