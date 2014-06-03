package org.specs2
package guide

import specification._
import core._

/**
 * base class for creating specs2 user guide pages.
 */
trait UserGuidePage extends Specification with UserGuideVariables with Snippets {
}

trait UserGuideVariables extends Specs2Variables {
  val triple = "\"\"\""
}