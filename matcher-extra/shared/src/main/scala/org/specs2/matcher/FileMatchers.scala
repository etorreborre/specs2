package org.specs2
package matcher

import text.Quote._
import io._
import scala.reflect.Selectable.reflectiveSelectable

/**
 * The PathMatchers trait provides matchers which are applicable to strings representing paths
 */
trait PathMatchers extends PathBaseMatchers with PathBeHaveMatchers
object PathMatchers extends PathMatchers

private[specs2]
trait PathBaseMatchers { outer =>
  private[specs2] val fileReader: FileReader = new org.specs2.io.FileReader {}
  import fileReader._

  /** matches if new File(path).exists */
  def beAnExistingPath = new PathMatcher((s: String) => exists(s), "exists", "doesn't exist")
  /** matches if new File(path).canRead */
  def beAReadablePath = new PathMatcher((s: String) => canRead(s), "is readable", "can't be read")
  /** matches if new File(path).canWrite */
  def beAWritablePath = new PathMatcher((s: String) => canWrite(s), "is writable", "can't be written")
  /** matches if new File(path).isAbsolute */
  def beAnAbsolutePath = new PathMatcher((s: String) => isAbsolute(s), "is absolute", "is not absolute")
  /** matches if new File(path).isHidden */
  def beAHiddenPath = new PathMatcher((s: String) => isHidden(s), "is hidden", "is not hidden")
  /** matches if new File(path).isFile */
  def beAFilePath = new PathMatcher((s: String) => isFile(s), "is a file", "is not a file")
  /** matches if new File(path).isDirectory  */
  def beADirectoryPath = new PathMatcher((s: String) => isDirectory(s), "is a directory", "is not a directory")
  /** matches if new File(path).getName == name */
  def havePathName(name: String) =
    new PathMatcher((s: String) => isEqualIgnoringSep(getName(s), name), "is named " + q(name), "is not named " + q(name))
  /** matches if new File(path).getAbsolutePath == absolutePath */
  def haveAsAbsolutePath(path: String) =
    new PathMatcher((s: String) => isEqualIgnoringSep(s, path), "has absolute path " + q(path), "doesn't have absolute path " + q(path))
  /** matches if new File(path).getCanonicalPath == canonicalPath */
  def haveAsCanonicalPath(path: String) =
    new PathMatcher((s: String) => isEqualIgnoringSep(getCanonicalPath(s), path), "has canonical path " + q(path), "doesn't have canonical path " + q(path))
  /** matches if new File(path).getParent == parent */
  def haveParentPath(parent: String) =
    new PathMatcher((s: String) => isEqualIgnoringSep(getParent(s), parent), "has parent path " + q(parent), "doesn't have parent path " + q(parent))
  /** matches if new File(path).list == list(files) */
  def listPaths(list: String*) =
    new PathMatcher((s: String) => list != null && listFiles(s).toList == list.toList, "has files " + q(list.mkString(", ")),
        "doesn't have files " + q(list.toList.mkString(", ")))
  /** matches if 2 paths are the same regardless of their separators */
  def beEqualToIgnoringSep(other: String) =
    new PathMatcher((s: String) => isEqualIgnoringSep(getCanonicalPath(s), other), "is equal ignoring separators to " + q(other),
        "is not equal ignoring separators to " + q(other))
  /** @return true if the 2 paths are equal, ignoring separators */
  private def isEqualIgnoringSep(path: String, other: String) = path != null && other != null&& getCanonicalPath(path).replaceAll("\\\\", "/") == getCanonicalPath(other).replaceAll("\\\\", "/")
}

object PathBaseMatchers extends PathBaseMatchers

private[specs2]
trait PathBeHaveMatchers extends BeHaveMatchers:
  private val outer = PathBaseMatchers

  /**
   * matcher aliases and implicits to use with be / have + matcher
   */
  extension (result: MatchResult[String]):
    def anExistingPath = result(outer.beAnExistingPath)
    def aHiddenPath = result(outer.beAHiddenPath)
    def aReadablePath = result(outer.beAReadablePath)
    def aWritablePath = result(outer.beAWritablePath)
    def anAbsolutePath = result(outer.beAnAbsolutePath)
    def aFilePath = result(outer.beAFilePath)
    def aDirectoryPath = result(outer.beADirectoryPath)
    def pathName(name: String) = result(outer.havePathName(name))
    def listPaths(list: String*) = result(outer.listPaths(list:_*))
    def asAbsolutePath(path: String) = result(outer.haveAsAbsolutePath(path))
    def asCanonicalPath(path: String) = result(outer.haveAsCanonicalPath(path))
    def parentPath(path: String) = result(outer.haveParentPath(path))
    def equalToIgnoringSep(other: String) = result(outer.beEqualToIgnoringSep(other))

  def anExistingPath = outer.beAnExistingPath
  def aHiddenPath = outer.beAHiddenPath
  def aReadablePath = outer.beAReadablePath
  def aWritablePath = outer.beAWritablePath
  def anAbsolutePath = outer.beAnAbsolutePath
  def aFilePath = outer.beAFilePath
  def aDirectoryPath = outer.beADirectoryPath
  def pathName(name: String) = outer.havePathName(name)
  def asAbsolutePath(name: String) = outer.haveAsAbsolutePath(name)
  def asCanonicalPath(name: String) = outer.haveAsCanonicalPath(name)
  def parentPath(parent: String) = outer.haveParentPath(parent)
  def equalToIgnoringSep(other: String) = outer.beEqualToIgnoringSep(other)

/**
 * The FileMatchers trait provides matchers which are applicable to files
 */
trait FileMatchers extends FileBaseMatchers with FileBeHaveMatchers
object FileMatchers extends FileMatchers

type HasPath = { def getPath(): String }

private[specs2]
trait FileBaseMatchers extends PathMatchers:
  /** matches if file.exists */
  def exist: Matcher[HasPath] = (beAnExistingPath) ^^ ((_:HasPath).getPath())
  /** matches if file.canRead */
  def beReadable: Matcher[HasPath] = (beAReadablePath) ^^ ((_:HasPath).getPath())
  /** matches if file.canWrite */
  def beWritable: Matcher[HasPath] = (beAWritablePath) ^^ ((_:HasPath).getPath())
  /** matches if file.isAbsolute */
  def beAbsolute: Matcher[HasPath] = (beAnAbsolutePath) ^^ ((_:HasPath).getPath())
  /** matches if file.isHidden */
  def beHidden: Matcher[HasPath] = (beAHiddenPath) ^^ ((_:HasPath).getPath())
  /** matches if file.isFile */
  def beAFile: Matcher[HasPath] = (beAFilePath) ^^ ((_:HasPath).getPath())
  /** matches if file.isDirectory */
  def beADirectory: Matcher[HasPath] = (beADirectoryPath) ^^ ((_:HasPath).getPath())
  /** matches if file.getName == name */
  def haveName(name: String): Matcher[HasPath] = (havePathName(name)) ^^ ((_:HasPath).getPath())
  /** matches if file.getAbsolutePath == path  */
  def haveAbsolutePath(path: String): Matcher[HasPath] = (haveAsAbsolutePath(path)) ^^ ((_:HasPath).getPath())
  /** matches if file.getCanonicalPath == path */
  def haveCanonicalPath(path: String): Matcher[HasPath] = (haveAsCanonicalPath(path)) ^^ ((_:HasPath).getPath())
  /** matches if file.getParent == path */
  def haveParent(path: String): Matcher[HasPath] = (haveParentPath(path)) ^^ ((_:HasPath).getPath())
  /** matches if file.list == list */
  def haveList(list: String): Matcher[HasPath] = (listPaths(list)) ^^ ((_:HasPath).getPath())

object FileBaseMatchers extends FileBaseMatchers

/**
 * This case class is used to provide the getPath() method,
 * so that all FileMatchers can be used on Strings.
 */
private[specs2]
case class Path(p: String):
  def path = this
  def getPath(): String = p

private[specs2]
trait FileBeHaveMatchers extends BeHaveMatchers {
  private val outer = FileBaseMatchers
  /**
   * matcher aliases and implicits to use with BeVerb and HaveVerb
   */
  extension (result: MatchResult[HasPath]):
    def hidden = result(outer.beHidden)
    def readable = result(outer.beReadable)
    def writable = result(outer.beWritable)
    def absolute = result(outer.beAbsolute)
    def aFile = result(outer.beAFile)
    def exist = result(outer.exist)
    def aDirectory = result(outer.beADirectory)
    def name(name: String) = result(outer.haveName(name))
    def paths(list: String) = result(outer.haveList(list))
    def absolutePath(path: String) = result(outer.haveAbsolutePath(path))
    def canonicalPath(path: String) = result(outer.haveCanonicalPath(path))
    def parent(path: String) = result(outer.haveParent(path))

  def hidden = outer.beHidden
  def readable = outer.beReadable
  def writable = outer.beWritable
  def absolute = outer.beAbsolute
  def aFile = outer.beAFile
  def aDirectory = outer.beADirectory
  def name(name: String) = outer.haveName(name)
  def paths(list: String) = outer.haveList(list)
  def absolutePath(path: String) = outer.haveAbsolutePath(path)
  def canonicalPath(path: String) = outer.haveCanonicalPath(path)
  def parent(path: String) = outer.haveParent(path)
}
private[specs2]
class PathMatcher(test: String => Boolean, ok: String, ko: String) extends Matcher[String]:
  def apply[S <: String](path: Expectable[S]) =
    result(path.value != null && test(path.value),
           path.description + " " + ok,
           path.description  + " " + ko,
           path)
