package org.specs2

import specification.{Text, SpecStart, Fragments}
import Specs2Variables._

/**
 * base class for creating specs2 user guide pages.
 *
 * If the text contains "${SPECS2_VERSION}", each occurrence will be replaced by the current specs2 version as defined in the build.sbt file
 * If the text contains "${SPECS2_BRANCH}", each occurrence will be replaced by either the official tag or master if the version is a SNAPSHOT one
 */
trait UserGuidePage extends Specification {
  override def map(fs: =>Fragments) =
    noindent ^ fs.map {
      case start @ SpecStart(_,_,_) if isIndex(start) => start.urlIs("index.html")
      case start @ SpecStart(_,_,_)                   => start.baseDirIs("./#{SPECS2_GUIDE}".replaceVariables)
      case Text(t)                                    => Text(t.replaceVariables)
      case other                                      => other
    }

  private def isIndex(start: SpecStart) = start.specName.javaClassName endsWith "Index"

  val triple = "\"\"\""
  val dollar = "$"
}