package org.specs2
package control


trait LanguageFeatures {
  implicit val implicitsAreAllowed = language.implicitConversions
  implicit val postfixOpsAreAllowed = language.postfixOps
}

trait NoLanguageFeatures extends LanguageFeatures {
  override implicit val implicitsAreAllowed = language.implicitConversions
  override implicit val postfixOpsAreAllowed = language.postfixOps
}
