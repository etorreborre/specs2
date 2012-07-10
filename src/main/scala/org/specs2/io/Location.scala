package org.specs2
package io

/**
 * Location of a Fragment in a file
 */
class Location {
  private val location = FromSource.location(fragmentFilter)
  def file: String = location.fileName
  def lineNumber: Int = location.lineNumber
  override def toString = location.fullLocation
  override def equals(a: Any) = a match {
    case l: Location => l.toString == this.toString
    case other       => false
  }

  private def isFragmentDefinition(s: String) =
    Seq("Described",
        "InExample"
  ).map("org.specs2.mutable.FragmentsBuilder$"+_).exists(s.contains)

  private def fragmentFilter = (st: Seq[StackTraceElement]) => {
    if (st.exists(_.toString.contains("org.specs2.mutable.FragmentsBuilder")))
      st.dropWhile(s => !isFragmentDefinition(s.toString)).
         dropWhile(s => isFragmentDefinition(s.toString))
    else
      st.dropWhile(_.toString.contains("org.specs2"))
  }
}
