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

    fragments.foldLeft((Vector(): Seq[TaggingFragment], (Section(), Tag()))) { (res, cur) =>
      val (tagged, (sectionTags, previousTag)) = res
      cur match {
        /** tag the next fragment */
        case t1 @ Tag(_*)                                        => (tagged :+ t1, (sectionTags, previousTag.add(t1)))
        /** tag the previous fragment */
        case t1 @ TaggedAs(_*)                                   => (tagged.mapLast(_ |+| Tag(t1.names:_*)) :+ t1, (sectionTags, previousTag))
        /** section for the next fragment */
        case t1 @ Section(_*) =>
          val (endTags, startTags) = (sectionTags.names.filter(t1.names.contains), t1.names.filterNot(sectionTags.names.contains))
          (tagged :+ Tag(startTags:_*), (Section((sectionTags.names diff endTags) ++ startTags:_*), previousTag))
        /** section for the previous fragment */
        case t1 @ AsSection(_*) =>
          val (endTags, startTags) = (sectionTags.names.filter(t1.names.contains), t1.names.filterNot(sectionTags.names.contains))
          (tagged.mapLast(_ |+| Tag(startTags:_*)) :+ t1, (Section((sectionTags.names diff endTags) ++ startTags:_*), previousTag))
        /** beginning of section from the previous fragment */
        case f                                                   => (tagged :+ Tag(sectionTags.names ++ previousTag.names:_*), (sectionTags, Tag()))
      }
    }
  }._1


}
