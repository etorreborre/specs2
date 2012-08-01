package org.specs2
package reporter

import org.specs2.internal.scalaz._
import Scalaz._
import specification._
import SpecsArguments._
import main.Arguments

/**
 * The Selection trait implements the logic for filtering the fragments to execute
 */
trait Selection {
  /** function returning a specification with only some selected fragments */
  def select(implicit arguments: Arguments): SpecificationStructure => SpecificationStructure
}

/**
 * The DefaultSelection trait filters the fragments to execute by filtering Examples:
 *
 * - by tags
 * - according to their names
 * - according to their previous execution
 *
 * If the `isolated` argument is passed, then each example is executed in its own copy of the specification
 */
trait DefaultSelection extends ExamplesIsolation with TagSelection with StatusSelection with ExamplesSelection with Selection {


  /** select function returning a filtered seq of Fragments */
  def select(implicit arguments: Arguments): SpecificationStructure => SpecificationStructure = (spec: SpecificationStructure) => {
    // isolate examples if necessary, using the arguments of the current specification in case of included specifications
    val fs = SpecsArguments.foldAll(select(spec.content.fragments)(arguments)).filter(isolateExamples2)
    SpecificationStructure(fs)
  }

  /** select function returning a filtered seq of Fragments */
  def select(fragments: Seq[Fragment])(implicit arguments: Arguments = Arguments()): Seq[Fragment] = {
    SpecsArguments.foldAll(fragments).filter(filter(arguments))
  }

  /**
   * @return filter fragments depending on the command line arguments and the current arguments in the specification
   */
  def filter(implicit commandLineArgs: Arguments) = (fan: Seq[(Fragment, Arguments, SpecName)]) => {
    fan |> filterTags |> filterPrevious |> filterExamples
  }

}
object DefaultSelection extends DefaultSelection
