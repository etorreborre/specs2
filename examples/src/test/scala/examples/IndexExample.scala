package examples

import org.specs2._
import runner.SpecificationsFinder._

/**
 * This Specification shows how to create an Index with references to other specifications
 */
class IndexExample extends Specification { def is =

  examplesLinks("All specifications")

  /**
   * @see the SpecificationsFinder trait for the parameters of the 'specifications' method
   *
   * use the `see` method to create a reference which will not re-trigger the execution of the linked specification
   * if it has already been executed
   */
  def examplesLinks(t: String) =
    t.title ^ specifications().map(s => link(s))

}
