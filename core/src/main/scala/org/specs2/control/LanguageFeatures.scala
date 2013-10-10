package org.specs2
package control


trait LanguageFeatures {
  implicit lazy val implicitsAreAllowed = language.implicitConversions
  implicit lazy val postfixOpsAreAllowed = language.postfixOps
}

trait NoLanguageFeatures extends LanguageFeatures {
  override implicit lazy val implicitsAreAllowed = language.implicitConversions
  override implicit lazy val postfixOpsAreAllowed = language.postfixOps
}
