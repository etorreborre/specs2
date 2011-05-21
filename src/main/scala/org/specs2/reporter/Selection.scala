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
  def select(fragments: Seq[Fragment])(implicit commandLineArgs: Arguments = Arguments()): Seq[Fragment] = {
    SpecsArguments.foldAll(fragments).filter(filter(commandLineArgs))
  }

  /**
   * @return filter fragments depending on the command line arguments and the current arguments in the specification
   */
  def filter(implicit commandLineArgs: Arguments) = (fragmentsAndArguments: Seq[(Fragment, Arguments)]) => {
    fragmentsAndArguments |> filterTags |> filterExamples
  }

  /**
   * @return filter fragments according to tags by collecting tags as applicable to each fragment and applying them
   */
  def filterTags(implicit commandLineArgs: Arguments) = (fragmentsAndArguments: Seq[(Fragment, Arguments)]) => {
    val fragments = fragmentsAndArguments.map(_._1)
    fragmentsAndArguments.zip(tags(fragments)) collect {
      case ((f, a), t) if !isTag(f) && t.keep(a.overrideWith(commandLineArgs)) => (f, a)
      case ((f @ SpecStart(_, _), a), t)                                       => (f, a)
      case ((f @ SpecEnd(_), a), t)                                            => (f, a)
    }
  }

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
        case f                                                  => (tagged :+ ma(taggingToApply).sum, taggingToApply.filter(_.isSection))
      }
    }
  }._1
  /** 
   * the filter method filters examples based on their description,
   * keeping only the ones matching the ex attribute of the arguments object
   */
  protected def filterExamples(implicit commandLineArgs: Arguments) = (fragmentsAndArguments: Seq[(Fragment, Arguments)]) => {
    fragmentsAndArguments filter {
      case (e @ Example(_, _), args) => e.matches(args.overrideWith(commandLineArgs).ex)
      case (f, args)                 => true
    } collect { case (f, a) => f }
  }
}
object DefaultSelection extends DefaultSelection
