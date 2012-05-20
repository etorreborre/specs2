package org.specs2
package reporter

import main.Arguments
import specification.TagsFragments._
import specification.{SpecEnd, SpecStart, SpecName, Fragment}
import org.specs2.internal.scalaz._
import Scalaz._
import Foldable._
import collection.Iterablex._

/**
 * This trait selects fragments based on their tags
 */
trait TagSelection {
  /**
   * @return filter fragments according to tags by collecting tags as applicable to each fragment and applying them
   */
  def filterTags(implicit commandLineArgs: Arguments) = (fan: Seq[(Fragment, Arguments, SpecName)]) => {
    val fragments = fan.map(_._1)
    fan.zip(tags(fragments)) collect {
      case ((f, a, n), t) if !isTag(f) && t.keep(a.overrideWith(commandLineArgs)) => (f, a, n)
      case ((f @ SpecStart(_,_,_), a, n), t)                                      => (f, a, n)
      case ((f @ SpecEnd(_,_), a, n), t)                                          => (f, a, n)
    }
  }

  /**
   * From a Seq of Fragments create a seq of corresponding tags for each fragment, considering that:
   *
   *  - a `TaggedAs` fragment is applicable to the the previous fragment
   *  - a `Tag` fragment is applicable to the the next fragment
   *  - a `AsSection` fragment is applicable to the the previous fragment to the next `AsSection` fragment with the same name
   *  - a `Section` fragment is applicable to the the next fragment to the next `Section` fragment with the same name
   */
  def tags(fragments: Seq[Fragment]): Seq[TaggingFragment] = {

    def removeTags(taggingToApply: Seq[TaggingFragment], tf: TaggingFragment) = taggingToApply.map { t =>
      t match {
        case s @ Section(_)   => Section((s.names diff tf.names):_*)
        case s @ AsSection(_) => AsSection((s.names diff tf.names):_*)
        case other            => t
      }
    }

    fragments.foldLeft((Vector(), Vector()): (Seq[TaggingFragment], Seq[TaggingFragment])) { (res, cur) =>
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

}
