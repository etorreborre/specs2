package org.specs2
package reporter

import main.Arguments
import control.LazyParameters._
import specification._
import Fragments._
import SpecsArguments._
import TagsFragments._
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
    SpecsArguments.foldAll(fragments).filter(filterTags, filterFragment)(arguments)
  }

  /**
   * @return filter fragments according to tags
   */
  def filterTags(implicit commandLineArgs: Arguments=Arguments()) = (fragmentsAndArguments: Seq[(Fragment, Arguments)]) => {
    val fragments = fragmentsAndArguments.map(_._2)
    fragmentsAndArguments.zip(fragments.drop(1) :+ fragments.take(1)).filterNot {
      case ((f, args), t @ TaggedAs(_)) if !t.keep(args.overrideWith(commandLineArgs)) => true
      case ((f, args), TaggedAs(_))                      => true
      case _                                             => false
    }.collect { case (a, b) => a }
  }

  /** 
   * the filter method filters examples based on their description,
   * keeping only the ones matching the ex attribute of the arguments object
   */
  protected def filterFragment = (f: Fragment, args: Arguments) => {
    f match {
      case e @ Example(_, _) => e.matches(args.ex)
      case _ => true
    }
  }
}
object DefaultSelection extends DefaultSelection