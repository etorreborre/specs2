package org.specs2
package reporter

import main.Arguments
import specification._
import TagsFragments._

/**
 * This trait selects fragments based on their tags
 */
trait TagSelection extends TagsAssociation {
  /**
   * @return filter fragments according to tags by collecting tags as applicable to each fragment and applying them
   */
  def filterTags(implicit commandLineArgs: Arguments) = (fan: Seq[(Fragment, Arguments, SpecName)]) => {
    val fragments = fan.map(_._1)
    fan.zip(tags(fragments)) collect {
      case ((f, a, n), t) if !isTag(f) && t.keep(a.overrideWith(commandLineArgs)) => (f, a, n)
      case ((f: SpecStart, a, n), t)                                              => (f, a, n)
      case ((f: SpecEnd, a, n), t)                                                => (f, a, n)
    }
  }

}
