package org.specs2.text

object Whitespace:

  def showStringWhitespaces(s: String): String =
    showStringTabs(showStringNewlines(showStringSpaces(s)))

  def showStringSpaces(s: String): String =
    s.replace(" ", "\u23B5")

  def showStringNewlines(s: String): String =
    s.replace("\n", "\u21B5")

  def showStringTabs(s: String): String =
    s.replace("\t", "\u2192")

  extension [T] (t: T)
    def showWhitespaces: String =
      Whitespace.showStringWhitespaces(t.toString)

    def showSpaces: String =
      Whitespace.showStringSpaces(t.toString)

    def showNewlines: String =
      Whitespace.showStringNewlines(t.toString)

    def showTabs: String =
      Whitespace.showStringTabs(t.toString)
