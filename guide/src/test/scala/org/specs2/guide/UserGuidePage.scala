package org.specs2
package guide

import org.specs2.form.Card
import specification._
/**
 * base class for creating specs2 user guide pages.
 */
abstract class UserGuidePage extends Specification with UserGuideVariables with Snippets

abstract class UserGuideCard extends Card with UserGuideVariables
