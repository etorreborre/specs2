package org.specs2.guide

import org.specs2.Specification
import org.specs2.specification.{Snippets, SpecStart, Fragments}

/**
 * base class for creating specs2 user guide pages.
 *
 * If the text contains "${VERSION}", each occurrence will be replaced by the current specs2 version as defined in the build.sbt file
 * If the text contains "${BRANCH}", each occurrence will be replaced by either the official tag or master if the version is a SNAPSHOT one
 */
trait UserGuidePage extends Specification with UserGuideVariables with Snippets {
  override def map(fs: =>Fragments) =
    noindent ^ fs.map {
      case start @ SpecStart(_,_,_) if isIndex(start) => start.urlIs("index.html")
      case start @ SpecStart(_,_,_)                   => start.baseDirIs(s"./$GUIDE_DIR")
      case other                                      => other
    }

  private def isIndex(start: SpecStart) = start.specName.javaClassName endsWith "Index"

}

trait UserGuideVariables extends Specs2Variables {
  val triple = "\"\"\""
}