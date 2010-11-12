package org.specs2
package io

private[specs2]
trait Paths { outer =>
  implicit def toPath(s: String) = Path(s)
  case class Path(s: String) {
    def dirPath = outer.dirPath(s)
    def normalize = outer.normalize(s)
  }
  def dirPath(s: String) = {
    val normalized = s.normalize
    if (normalized.endsWith("/")) normalized
    else normalized + "/"
  }
  def normalize(s: String) = s.replace("\\", "/")
}

private[specs2]
object Paths extends Paths