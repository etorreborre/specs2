package org.specs2
package specification

import TagsFragments._
import scalaz.{syntax, Foldable, Scalaz}
import Scalaz._
import collection.Iterablex._
import collection.Seqx._

trait TagsAssociation {
  /**
   * Associate each fragment with its tag according to the "tags" method
   */
  private[specs2]
  def tagFragments(fragments: Seq[Fragment]): Seq[(Fragment, TaggingFragment)] = fragments zip tags(fragments)

  /**
   * From a Seq of Fragments create a seq of corresponding tags for each fragment, considering that:
   *
   *  - a `TaggedAs` fragment is applicable to the the previous fragment
   *  - a `Tag` fragment is applicable to the the next fragment
   *  - a `AsSection` fragment is applicable to the the previous fragment to the next `AsSection` fragment with the same name
   *  - a `Section` fragment is applicable to the the next fragment to the next `Section` fragment with the same name
   */
  private[specs2]
  def tags(fragments: Seq[Fragment]): Seq[TaggingFragment] = {

    def removeTags(taggingToApply: Seq[TaggingFragment], tf: TaggingFragment) = taggingToApply.collect {
      case s @ Section(_*)   => Section((s.names diff tf.names):_*)
      case s @ AsSection(_*) => AsSection((s.names diff tf.names):_*)
      case other             => other
    }.filterNot(_.isEmpty)

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
        case f                                                   => (tagged :+ taggingToApply.sumr, taggingToApply.filter(_.isSection))
      }
    }
  }._1


}
