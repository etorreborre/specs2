package org.specs2
package guide

import form.Card
import specification.core.Fragments
import specification._
/**
 * base class for creating specs2 user guide pages.
 */
abstract class UserGuidePage extends Specification with UserGuideVariables with Snippets with Forms {
  override def map(fs: =>Fragments) = super.map(fs.compact)
}

abstract class UserGuideCard extends Card with UserGuideVariables with Forms
