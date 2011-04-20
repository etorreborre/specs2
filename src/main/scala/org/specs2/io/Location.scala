package org.specs2
package io

/**
 * Location of a Fragment in a file
 */
class Location {
  private val location = FromSource.location
  def file: String = location.fileName
  def lineNumber: Int = location.lineNumber
  override def toString = location.fullLocation
}