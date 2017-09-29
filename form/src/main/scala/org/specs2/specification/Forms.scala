package org.specs2
package specification

import create._
import form._
import dsl.FormDsl

/**
 * Allow a Form to be inserted among Fragments as a Text Fragment
 * Allow a Form to be used as an example body and return a Result automatically
 */
trait Forms extends FormsBuilder
  with FormFragmentsFactory
  with DecoratedProperties
  with FormS2StringContext
  with FormDsl
  with PropSyntax

object Forms extends Forms

