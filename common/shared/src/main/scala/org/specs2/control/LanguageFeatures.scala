package org.specs2
package control

/**
 * implicits and postfix ops are automatically mixed in specs2 specifications
 * for convenience. If you *really* don't want that you can override this behaviour by using the NoLanguageFeatures trait
 */
trait LanguageFeatures {
  implicit lazy val implicitsAreAllowed = language.implicitConversions
  implicit lazy val postfixOpsAreAllowed = language.postfixOps
}

trait NoLanguageFeatures extends LanguageFeatures {
  override lazy val implicitsAreAllowed = language.implicitConversions
  override lazy val postfixOpsAreAllowed = language.postfixOps
}
