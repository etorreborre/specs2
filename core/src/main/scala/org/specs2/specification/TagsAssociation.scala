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
    val (tags, _) =
    fragments.foldLeft((Vector(): Seq[TaggingFragment], (Seq(): Seq[TaggingFragment], AlwaysWhenNoIncludeTag: TaggingFragment))) { (res, cur) =>
      val (tagged, (sectionTags, previousTag)) = res
      cur match {
        case t1: TaggingFragment if !t1.isSection =>
          if (t1.isTaggingNext) (tagged :+ t1,                   (sectionTags, previousTag |+| t1))
          else                  (tagged.mapLast(_ |+| t1) :+ t1, (sectionTags, previousTag))

        /** section for the next fragment */
        case t1: TaggingFragment =>
          val endTags        = sectionTags.filter(_.names.exists(t1.names.contains))
          val startTags      = sectionTags.map(t => t.removeNames(t1.names)).filterNot(_.names.isEmpty)
          val newSectionTags = if (endTags.isEmpty) startTags :+ t1 else startTags
          val tagToApply     = startTags.sumr |+| t1

          if (t1.isTaggingNext) (tagged :+ tagToApply,                   (newSectionTags, t1))
          else                  (tagged.mapLast(_ |+| tagToApply) :+ t1, (newSectionTags, previousTag))

        /** beginning of section from the previous fragment */
        case f => (tagged :+ (sectionTags.sumr |+| previousTag), (sectionTags, AlwaysWhenNoIncludeTag))
      }
    }
    tags
  }


}
