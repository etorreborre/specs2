package org.specs2
package matcher

import text.Quote._
import io._
import scala.reflect.Selectable.reflectiveSelectable
import execute._, Result._
import Matcher.{given}

/**
 * The PathMatchers trait provides matchers which are applicable to strings representing paths
 */
trait PathMatchers:
  outer =>

  private[specs2] val fileReader: FileReader = new org.specs2.io.FileReader {}
  import fileReader._

  /** matches if new File(path).exists */
  def beAnExistingPath: PathMatcher =
    new PathMatcher((s: String) => exists(s), "doesn't exist")

  /** matches if new File(path).canRead */
  def beAReadablePath: PathMatcher =
    new PathMatcher((s: String) => canRead(s), "can't be read")

  /** matches if new File(path).canWrite */
  def beAWritablePath: PathMatcher =
    new PathMatcher((s: String) => canWrite(s), "can't be written")

  /** matches if new File(path).isAbsolute */
  def beAnAbsolutePath: PathMatcher =
    new PathMatcher((s: String) => isAbsolute(s), "is not absolute")

  /** matches if new File(path).isHidden */
  def beAHiddenPath: PathMatcher =
    new PathMatcher((s: String) => isHidden(s), "is not hidden")

  /** matches if new File(path).isFile */
  def beAFilePath: PathMatcher =
    new PathMatcher((s: String) => isFile(s), "is not a file")

  /** matches if new File(path).isDirectory  */
  def beADirectoryPath: PathMatcher =
    new PathMatcher((s: String) => isDirectory(s), "is not a directory")

  /** matches if new File(path).getName == name */
  def havePathName(name: String): PathMatcher =
    new PathMatcher((s: String) => isEqualIgnoringSep(getName(s), name), "is not named " + q(name))

  /** matches if new File(path).getAbsolutePath == absolutePath */
  def haveAsAbsolutePath(path: String): PathMatcher =
    new PathMatcher((s: String) => isEqualIgnoringSep(s, path), "doesn't have absolute path " + q(path))

  /** matches if new File(path).getCanonicalPath == canonicalPath */
  def haveAsCanonicalPath(path: String): PathMatcher =
    new PathMatcher((s: String) => isEqualIgnoringSep(getCanonicalPath(s), path), "doesn't have canonical path " + q(path))

  /** matches if new File(path).getParent == parent */
  def haveParentPath(parent: String): PathMatcher =
    new PathMatcher((s: String) => isEqualIgnoringSep(getParent(s), parent), "doesn't have parent path " + q(parent))

  /** matches if new File(path).list == list(files) */
  def listPaths(list: String*): PathMatcher =
    new PathMatcher((s: String) => list != null && listFiles(s).toList == list.toList,
        "doesn't have files " + q(list.toList.mkString(", ")))

  /** matches if 2 paths are the same regardless of their separators */
  def beEqualToIgnoringSep(other: String): PathMatcher =
    new PathMatcher((s: String) => isEqualIgnoringSep(getCanonicalPath(s), other),
        "is not equal ignoring separators to " + q(other))

  /** @return true if the 2 paths are equal, ignoring separators */
  private def isEqualIgnoringSep(path: String, other: String): Boolean =
    path != null && other != null &&
    getCanonicalPath(path).replaceAll("\\\\", "/") == getCanonicalPath(other).replaceAll("\\\\", "/")

object PathMatchers extends PathMatchers

/**
 * The FileMatchers trait provides matchers which are applicable to files
 */
trait FileMatchers extends PathMatchers:
  /** matches if file.exists */
  def exist: Matcher[HasPath] =
    (beAnExistingPath) ^^ ((_:HasPath).getPath())

  /** matches if file.canRead */
  def beReadable: Matcher[HasPath] =
    (beAReadablePath) ^^ ((_:HasPath).getPath())

  /** matches if file.canWrite */
  def beWritable: Matcher[HasPath] =
    (beAWritablePath) ^^ ((_:HasPath).getPath())

  /** matches if file.isAbsolute */
  def beAbsolute: Matcher[HasPath] =
    (beAnAbsolutePath) ^^ ((_:HasPath).getPath())

  /** matches if file.isHidden */
  def beHidden: Matcher[HasPath] =
    (beAHiddenPath) ^^ ((_:HasPath).getPath())

  /** matches if file.isFile */
  def beAFile: Matcher[HasPath] =
    (beAFilePath) ^^ ((_:HasPath).getPath())

  /** matches if file.isDirectory */
  def beADirectory: Matcher[HasPath] =
    (beADirectoryPath) ^^ ((_:HasPath).getPath())

  /** matches if file.getName == name */
  def haveName(name: String): Matcher[HasPath] =
    (havePathName(name)) ^^ ((_:HasPath).getPath())

  /** matches if file.getAbsolutePath == path  */
  def haveAbsolutePath(path: String): Matcher[HasPath] =
    (haveAsAbsolutePath(path)) ^^ ((_:HasPath).getPath())

  /** matches if file.getCanonicalPath == path */
  def haveCanonicalPath(path: String): Matcher[HasPath] =
    (haveAsCanonicalPath(path)) ^^ ((_:HasPath).getPath())

  /** matches if file.getParent == path */
  def haveParent(path: String): Matcher[HasPath] =
    (haveParentPath(path)) ^^ ((_:HasPath).getPath())

  /** matches if file.list == list */
  def haveList(list: String): Matcher[HasPath] =
    (listPaths(list)) ^^ ((_:HasPath).getPath())

object FileMatchers extends FileMatchers

type HasPath = { def getPath(): String }

/**
 * This case class is used to provide the getPath() method,
 * so that all FileMatchers can be used on Strings.
 */
private[specs2]
case class Path(p: String):
  def path = this
  def getPath(): String = p

private[specs2]
class PathMatcher(test: String => Boolean, ko: String) extends Matcher[String]:
  def apply[S <: String](path: Expectable[S]) =
    result(path.value != null && test(path.value),
           path.description  + " " + ko)
