package org.specs2
package io

/**
 * Location of a Fragment in a file
 */
class Location {
  private val location = FromSource.location(st => st.filterNot(_.toString.contains("org.specs2")))
  def file: String = location.fileName
  def lineNumber: Int = location.lineNumber
  override def toString = location.fullLocation
  override def equals(a: Any) = a match {
    case l: Location => l.toString == this.toString
    case other       => false
  }
}
