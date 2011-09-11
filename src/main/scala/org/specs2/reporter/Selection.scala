package org.specs2
package reporter

import org.specs2.internal.scalaz._
import Scalaz._
import collection.Iterablex._
import main.Arguments
import control.LazyParameters._
import specification._
import Fragments._
import SpecsArguments._
import Foldable._
import TagsFragments._
/**
 * The Selection trait implements the logic for filtering the fragments to execute
 */
trait Selection {
  /** select function returning a specification with filtered */
  def select(implicit arguments: Arguments): SpecificationStructure => SpecificationStructure
}

/**
 * The DefaultSelection trait filters the fragments to execute by filtering Examples according to their names
 *
 * It possibly filters out the previously executed fragments based on their status
 */
trait DefaultSelection extends WithDefaultStatisticsRepository {


  /** select function returning a filtered seq of Fragments */
  def select(implicit arguments: Arguments): SpecificationStructure => SpecificationStructure = (spec: SpecificationStructure) =>
    SpecificationStructure(select(spec.content.fragments)(arguments))

  /** select function returning a filtered seq of Fragments */
  def select(fragments: Seq[Fragment])(implicit commandLineArgs: Arguments = Arguments()): Seq[Fragment] = {
    SpecsArguments.foldAll(fragments).filter(filter(commandLineArgs))
  }

  /**
   * @return filter fragments depending on the command line arguments and the current arguments in the specification
   */
  def filter(implicit commandLineArgs: Arguments) = (fan: Seq[(Fragment, Arguments, SpecName)]) => {
    fan |> filterTags |> filterPrevious |> filterExamples
  }

  /**
   * @return filter fragments according to tags by collecting tags as applicable to each fragment and applying them
   */
  def filterTags(implicit commandLineArgs: Arguments) = (fan: Seq[(Fragment, Arguments, SpecName)]) => {
    val fragments = fan.map(_._1)
    fan.zip(tags(fragments)) collect {
      case ((f, a, n), t) if !isTag(f) && t.keep(a.overrideWith(commandLineArgs)) => (f, a, n)
      case ((f @ SpecStart(_,_,_,_), a, n), t)                                    => (f, a, n)
      case ((f @ SpecEnd(_), a, n), t)                                            => (f, a, n)
    }
  }

  /**
   * @return filter fragments according to their previous execution state
   */
  def filterPrevious(implicit commandLineArgs: Arguments) = (fan: Seq[(Fragment, Arguments, SpecName)]) => {
    if (commandLineArgs.store.never) fan
    else
      fan filter {
        case (e @ Example(_, _), args, specName) => {
          val currentArgs = args.overrideWith(commandLineArgs)
          !currentArgs.wasIsDefined || includePrevious(specName, e, currentArgs)
        }
        case other => true
      }
  }

  protected def includePrevious(specName: SpecName, e: Example, args: Arguments) =
    args.was(repository.previousResult(specName, e).map(_.status).getOrElse(""))
  
  /**
   * From a Seq of Fragments create a seq of corresponding tags for each fragment, considering that:
   *
   *  * a `TaggedAs` fragment is applicable to the the previous fragment
   *  * a `Tag` fragment is applicable to the the next fragment
   *  * a `AsSection` fragment is applicable to the the previous fragment to the next `AsSection` fragment with the same name
   *  * a `Section` fragment is applicable to the the next fragment to the next `Section` fragment with the same name
   */
  def tags(fragments: Seq[Fragment]): Seq[TaggingFragment] = {

    def removeTags(taggingToApply: Seq[TaggingFragment], tf: TaggingFragment) = taggingToApply.map { t =>
        t match {
          case s @ Section(_)   => Section((s.names diff tf.names):_*)
          case s @ AsSection(_) => AsSection((s.names diff tf.names):_*)
          case other            => t
        }
      }

    fragments.foldLeft((Nil, Nil): (Seq[TaggingFragment], Seq[TaggingFragment])) { (res, cur) =>
      val (tagged, taggingToApply) = res
      cur match {
        /** tag the next fragment */
        case t1 @ Tag(_*)                                        => (tagged :+ t1, taggingToApply :+ t1)
        /** beginning of section */
        case t1 @ Section(_*) if !(taggingToApply contains t1)   => (tagged :+ Tag(t1.names:_*), taggingToApply :+ t1)
        /** end of section */
        case t1 @ Section(_*)                                    => (tagged :+ Tag(t1.names:_*), removeTags(taggingToApply, t1))
        /** tag the previous fragment */
        case t1 @ TaggedAs(_*)                                   => (tagged.mapLast(_ |+| Tag(t1.names:_*)) :+ t1, taggingToApply)
        /** beginning of section from the previous fragment */
        case t1 @ AsSection(_*) if !(taggingToApply contains t1) => (tagged.mapLast(_ |+| Tag(t1.names:_*)) :+ t1, taggingToApply :+ t1)
        /** end of section */
        case t1 @ AsSection(_*)                                  => (tagged.mapLast(_ |+| Tag(t1.names:_*)) :+ t1, removeTags(taggingToApply, t1))
        /** beginning of section from the previous fragment */
        case f                                                   => (tagged :+ ma(taggingToApply).sum, taggingToApply.filter(_.isSection))
      }
    }
  }._1
  /** 
   * the filter method filters examples based on their description,
   * keeping only the ones matching the ex attribute of the arguments object
   */
  protected def filterExamples(implicit commandLineArgs: Arguments) = (fan: Seq[(Fragment, Arguments, SpecName)]) => {
    fan filter {
      case (e @ Example(_, _), args, n) => e.matches(args.overrideWith(commandLineArgs).ex)
      case (f, args, n)                 => true
    } collect { case (f, a, n) => f }
  }
}
object DefaultSelection extends DefaultSelection
