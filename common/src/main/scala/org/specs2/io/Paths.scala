package org.specs2
package io

/**
 * This trait provides implicit conversions from strings to a Path object providing path-related functions like
 * setting all the separators as UNIX separators.
 */
trait Paths { outer =>
  implicit class Path(s: String) {
    def unixize = s.replace("\\", "/")
  }
}


object Paths extends Paths
