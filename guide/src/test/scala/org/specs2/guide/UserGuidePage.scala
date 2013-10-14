package org.specs2.guide

import org.specs2.Specification
import org.specs2.specification.{Snippets, SpecStart, Fragments}

/**
 * base class for creating specs2 user guide pages.
 */
trait UserGuidePage extends Specification with UserGuideVariables with Snippets {
  override def map(fs: =>Fragments) =
    Fragments.create(fs.fragments.map {
      case start: SpecStart if isIndex(start) => start.urlIs("index.html")
      case start: SpecStart                   => start.baseDirIs(s"./$GUIDE_DIR")
      case other                              => other
    }:_*)

  private def isIndex(start: SpecStart) = start.specName.javaClassName endsWith "Index"

}

trait UserGuideVariables extends Specs2Variables {
  val triple = "\"\"\""
}