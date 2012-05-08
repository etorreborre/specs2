package org.specs2
package io

import java.io.File
import java.net.URI
import control.Exceptions._
import text.Trim._
/**
 * This trait provides implicit conversions from strings to a Path object providing path-related functions like
 * setting all the separators as UNIX separators.
 */
private[specs2]
trait Paths { outer =>
  implicit def toPath(s: String) = Path(s)
  def dirPath(s: String) = {
    val normalized = s.normalize
    if (normalized.endsWith("/")) normalized
    else normalized + "/"
  }
  def fileName(p: String) = new File(p).getName
  def unixize(s: String)   = s.replace("\\", "/")
  def normalize(s: String) = s.unixize.removeFirst("\\./")
  def baseDir(s: String) = "./"+("../" * (s.normalize.split("/").filterNot(_.isEmpty).size - 1))
  def parentDir(s: String) = Option(new File(s.normalize).getParent).getOrElse("").dirPath
  def rebase(s: String, dir: String) = dir.dirPath + new File(s).getName
  def uriEncode(url: String) = tryo(new URI("http", "", "/"+url, null).toASCIIString.replace("http:///", "")).getOrElse(url)
  def isRelative(s: String) = Seq("./", "../").exists(s.unixize.startsWith)
  def relativeTo(p1: String, p2: String) = (p2.baseDir+p1).normalize
  def unrelativeTo(p1: String, p2: String) = if (p1.isRelative) p2.parentDir + p1.fileName else p1
  def fromTop(s: String) = relativeTo(s, s)
}

case class Path(s: String) {
  def dirPath = Paths.dirPath(s)
  def fileName = Paths.fileName(s)
  def parentDir = Paths.parentDir(s)
  def unixize = Paths.unixize(s)
  def normalize = Paths.normalize(s)
  def baseDir = Paths.baseDir(s)
  def rebase(dir: String) = Paths.rebase(s, dir)
  def uriEncode = Paths.uriEncode(s)
  def samePathAs(o: String) = fs.samePath(s, o)
  def isRelative = Paths.isRelative(s)
  def relativeTo(path: String) = Paths.relativeTo(s, path)
  def unrelativeTo(path: String) = Paths.unrelativeTo(s, path)
  def fromTop = Paths.fromTop(s)
}

private[specs2]
object Paths extends Paths