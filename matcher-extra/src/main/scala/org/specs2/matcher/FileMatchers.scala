package org.specs2
package matcher

import text.Quote._
import io._
/**
 * The PathMatchers trait provides matchers which are applicable to strings representing paths
 */
trait PathMatchers extends PathBaseMatchers with PathBeHaveMatchers
object PathMatchers extends PathMatchers

private[specs2]
trait PathBaseMatchers { outer =>
  private[specs2] val fileSystem: FileReader = new org.specs2.io.FileReader {}
  import fileSystem._

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

private[specs2]
trait PathBeHaveMatchers { outer: PathBaseMatchers =>
  /** 
   * matcher aliases and implicits to use with be / have + matcher
   */
  implicit def toPathResultMatcher(result: MatchResult[String]) = new PathResultMatcher(result)
  class PathResultMatcher(result: MatchResult[String]) {
    def anExistingPath = result(outer.beAnExistingPath)
    def aHiddenPath = result(outer.beAHiddenPath)
    def aReadablePath = result(outer.beAReadablePath)
    def aWritablePath = result(outer.beAWritablePath)
    def anAbsolutePath = result(outer.beAnAbsolutePath)
    def aFilePath = result(outer.beAFilePath)
    def aDirectoryPath = result(outer.beADirectoryPath)
    def pathName(name: String) = result(havePathName(name))
    def listPaths(list: String*) = result(outer.listPaths(list:_*))
    def asAbsolutePath(path: String) = result(haveAsAbsolutePath(path))
    def asCanonicalPath(path: String) = result(haveAsCanonicalPath(path))
    def parentPath(path: String) = result(haveParentPath(path))
    def equalToIgnoringSep(other: String) = result(beEqualToIgnoringSep(other))
  }
  def anExistingPath = beAnExistingPath 
  def aHiddenPath = beAHiddenPath
  def aReadablePath = beAReadablePath
  def aWritablePath = beAWritablePath
  def anAbsolutePath = beAnAbsolutePath
  def aFilePath = beAFilePath
  def aDirectoryPath = beADirectoryPath
  def pathName(name: String) = havePathName(name)
  def asAbsolutePath(name: String) = haveAsAbsolutePath(name) 
  def asCanonicalPath(name: String) = haveAsCanonicalPath(name) 
  def parentPath(parent: String) = haveParentPath(parent) 
  def equalToIgnoringSep(other: String) = beEqualToIgnoringSep(other)
}
/**
 * The FileMatchers trait provides matchers which are applicable to files
 */
trait FileMatchers extends FileBaseMatchers with FileBeHaveMatchers
object FileMatchers extends FileMatchers

private[specs2]
trait FileBaseMatchers extends PathMatchers {
  /** matches if file.exists */   
  def exist[T <: { def getPath(): String }] = (beAnExistingPath) ^^ ((_:T).getPath)
  /** matches if file.canRead */   
  def beReadable[T <: { def getPath(): String }] = (beAReadablePath) ^^ ((_:T).getPath)
  /** matches if file.canWrite */   
  def beWritable[T <: { def getPath(): String }] = (beAWritablePath) ^^ ((_:T).getPath)
  /** matches if file.isAbsolute */   
  def beAbsolute[T <: { def getPath(): String }] = (beAnAbsolutePath) ^^ ((_:T).getPath)
  /** matches if file.isHidden */   
  def beHidden[T <: { def getPath(): String }] = (beAHiddenPath) ^^ ((_:T).getPath)
  /** matches if file.isFile */   
  def beAFile[T <: { def getPath(): String }] = (beAFilePath) ^^ ((_:T).getPath)
  /** matches if file.isDirectory */   
  def beADirectory[T <: { def getPath(): String }] = (beADirectoryPath) ^^ ((_:T).getPath)
  /** matches if file.getName == name */   
  def haveName[T <: { def getPath(): String }](name: String) = (havePathName(name)) ^^ ((_:T).getPath)
  /** matches if file.getAbsolutePath == path  */   
  def haveAbsolutePath[T <: { def getPath(): String }](path: String) = (haveAsAbsolutePath(path)) ^^ ((_:T).getPath)
  /** matches if file.getCanonicalPath == path */   
  def haveCanonicalPath[T <: { def getPath(): String }](path: String) = (haveAsCanonicalPath(path)) ^^ ((_:T).getPath)
  /** matches if file.getParent == path */   
  def haveParent[T <: { def getPath(): String }](path: String) = (haveParentPath(path)) ^^ ((_:T).getPath)
  /** matches if file.list == list */   
  def haveList[T <: { def getPath(): String }](list: String) = (listPaths(list)) ^^ ((_:T).getPath)
  /**
   * transforms a string as a Path object to allow matches like:
   * "c:/projects".path must exist
   */
  private implicit def asPath(p: String) = Path(p)
}
/**
 * This case class is used to provide the getPath() method,
 * so that all FileMatchers can be used on Strings.
 */
private[specs2]
case class Path(p: String) {
  def path = this
  def getPath(): String = p
}
private[specs2]
trait FileBeHaveMatchers { this: FileBaseMatchers =>
  /** 
   * matcher aliases and implicits to use with BeVerb and HaveVerb 
   */
  implicit def toFileResultMatcher[T <: { def getPath(): String }](result: MatchResult[T]) = new FileResultMatcher(result)
  class FileResultMatcher[T <: { def getPath(): String }](result: MatchResult[T]) {
    def hidden = result(beHidden)
    def readable = result(beReadable)
    def writable = result(beWritable)
    def absolute = result(beAbsolute)
    def aFile = result(beAFile)
    def aDirectory = result(beADirectory)
    def name(name: String) = result(haveName(name))
    def paths(list: String) = result(haveList(list))
    def absolutePath(path: String) = result(haveAbsolutePath(path))
    def canonicalPath(path: String) = result(haveCanonicalPath(path))
    def parent(path: String) = result(haveParent(path))
  }
  def hidden[T <: { def getPath(): String }] = beHidden
  def readable[T <: { def getPath(): String }] = beReadable
  def writable[T <: { def getPath(): String }] = beWritable
  def absolute[T <: { def getPath(): String }] = beAbsolute
  def aFile[T <: { def getPath(): String }] = beAFile
  def aDirectory[T <: { def getPath(): String }] = beADirectory
  def name[T <: { def getPath(): String }](name: String) = haveName(name)
  def paths[T <: { def getPath(): String }](list: String) = haveList(list)
  def absolutePath[T <: { def getPath(): String }](path: String) = haveAbsolutePath(path)
  def canonicalPath[T <: { def getPath(): String }](path: String) = haveCanonicalPath(path)
  def parent[T <: { def getPath(): String }](path: String) = haveParent(path)
}
private[specs2]
class PathMatcher(test: String => Boolean, ok: String, ko: String) extends Matcher[String] { 
  def apply[S <: String](path: Expectable[S]) = {
    result(path.value != null && test(path.value),
           path.description + " " + ok, 
           path.description  + " " + ko,
           path)
  } 
} 
