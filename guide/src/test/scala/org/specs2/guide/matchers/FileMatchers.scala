package org.specs2
package guide
package matchers

object FileMatchers extends UserGuideCard {
  def title = "File"
  def text = s2"""
The Java api for files is more or less mimicked as matchers which can operate on strings denoting paths or on Files (with the `org.specs2.matcher.FileMatchers` trait)

 * `beEqualToIgnoringSep` check if 2 paths are the same regardless of their separators
 `"c:\temp\hello" must beEqualToIgnoringSep("c:/temp/hello")`
 * `beAnExistingPath` check if a path exists
 * `beAReadablePath` check if a path is readable
 * `beAWritablePath` check if a path is writable
 * `beAnAbsolutePath` check if a path is absolute
 * `beAHiddenPath` check if a path is hidden
 * `beAFilePath` check if a path is a file
 * `beADirectoryPath` check if a path is a directory
 * `havePathName` check if a path has a given name
 * `haveAsAbsolutePath` check if a path has a given absolute path
 * `haveAsCanonicalPath` check if a path has a given canonical path
 * `haveParentPath` check if a path has a given parent path
 * `listPaths` check if a path has a given list of children
 * `exist` check if a file exists

 * `beReadable` check if a file is readable
 * `beWritable` check if a file is writable
 * `beAbsolute` check if a file is absolute
 * `beHidden` check if a file is hidden
 * `beAFile` check if a file is a file
 * `beADirectory` check if a file is a directory
 * `haveName` check if a file has a given name
 * `haveAbsolutePath` check if a file has a given absolute path
 * `haveCanonicalPath` check if afile has a given canonical path
 * `haveParent` check if a file has a given parent path
 * `haveList` check if a file has a given list of children
"""
}
