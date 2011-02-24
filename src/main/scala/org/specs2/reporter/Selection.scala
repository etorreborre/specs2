package org.specs2
package reporter

import main.Arguments
import control.LazyParameters._
import specification._
import Fragments._
import SpecsArguments._

/**
 * The Selection trait implements the logic for filtering the fragments to execute
 */
trait Selection {
  /** select function returning a filtered and ordered seq of seq of Fragments */
  def select(implicit arguments: Arguments): Fragments => Seq[Fragment]
}

/**
 * The DefaultSelection trait filters the fragments to execute by filtering Examples according to their names
 */
trait DefaultSelection {
  /** select function returning a filtered seq of Fragments */
  def select(implicit arguments: Arguments): Fragments => Seq[Fragment] = (fragments: Fragments) => select(fragments.fragments)(arguments)
  /** select function returning a filtered seq of Fragments */
  def select(fragments: Seq[Fragment])(implicit arguments: Arguments = Arguments()): Seq[Fragment] = {
    SpecsArguments.filter(fragments)(filter)
  }
  /** 
   * the filter method filters examples based on their description,
   * keeping only the ones matching the ex attribute of the arguments object
   */
  protected def filter = (f: Fragment, args: Arguments) => {
    f match {
      case e @ Example(_, _) => e.matches(args.ex)
      case _ => true
    }
  }
}
