package org.specs2
package io

/**
 * This trait provides implicit conversions from strings to a Path object providing path-related functions like
 * setting all the separators as UNIX separators.
 */
private[specs2]
trait Paths:
  extension (s: String)
    def unixize: String =
      s.replace("\\", "/")

private[specs2]
object Paths extends Paths
