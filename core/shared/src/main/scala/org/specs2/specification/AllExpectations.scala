package org.specs2
package specification

import matcher._
import execute._
import main._
import specification.core._
import specification.create._

/**
 * This trait can be mixed-in a specification to allow examples to have all of their expectations being evaluated (unless
 * the example body throws an exception of course).
 *
 * All the results are collected into a list, provided by the StoredExpectations trait. These results form then the body
 * of the each example (decorated by a special ExampleFactory) so that each example returns a Result which is the summary
 * of all the individual issues.
 *
 * It must be noted that this trait relies on a mutable list to collect the results as they are created in the example body.
 * Because of this restriction, a Specification using that trait can either run sequentially or isolated.
 *
 * If the specification is neither sequential or isolated, we force it to be isolated by default.
 */
trait AllExpectations extends StoredExpectations with FragmentsFactory with SpecificationStructure with ArgumentsCreation {
  /**
   * @return an example factory which will take the stored results and make them the example result
   */
  implicit override protected def fragmentFactory: FragmentFactory =
    new ContextualFragmentFactory(super.fragmentFactory, (env: Env) => resultsContext(storedResults))

  /**
   * create a new Context with the list of captured results.
   *
   * This method could be overridden to filter the captured results and remove the skipped results for example
   */
  def resultsContext(results: => scala.collection.Seq[Result]): Context = new ResultsContext(results)

  /**
   * we force the specification to be isolated if it's not sequential or already isolated.
   * this is important because when an example runs, its results are being stored into a shared list
   */
  /** modify the specification structure */
  override def map(structure: SpecStructure): SpecStructure = {
    val arguments = structure.arguments
    if (arguments.isolated || arguments.sequential) structure
    else structure.setArguments(arguments <| args(isolated = ArgProperty(true)))
  }
}

