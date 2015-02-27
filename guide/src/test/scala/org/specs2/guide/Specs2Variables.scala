package org.specs2
package guide

import info.guide.BuildInfo

object Specs2Variables extends Specs2Variables

trait Specs2Variables {

  lazy val VERSION        = BuildInfo.version
  lazy val DOC_DIR        = s"http://etorreborre.github.io/specs2/SPECS2-$VERSION"
  lazy val PROJECT_DIR    = s"https://github.com/etorreborre/specs2/tree/SPECS2-$VERSION"
  lazy val GUIDE_DIR      = s"$DOC_DIR/guide"
  lazy val EXAMPLES_DIR   = s"$PROJECT_DIR/examples/src/test/scala/examples"
  lazy val API_DIR        = s"$DOC_DIR/api"

  lazy val specs2Variables: Map[String, String] =
    Map(
      "VERSION"           -> VERSION,
      "DOC_DIR"           -> DOC_DIR,
      "PROJECT_DIR"       -> PROJECT_DIR,
      "GUIDE_DIR"         -> GUIDE_DIR,
      "EXAMPLES_DIR"      -> EXAMPLES_DIR,
      "API_DIR"           -> API_DIR
    )

}