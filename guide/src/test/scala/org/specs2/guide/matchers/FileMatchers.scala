package org.specs2
package guide
package matchers

object FileMatchers extends UserGuideCard {
  def title = "File"
  def text = s2"""
The Java api for files is more or less mimicked as matchers which can operate on strings denoting paths or on Files (with the `org.specs2.matcher.FileMatchers` trait)

 * `beEqualToIgnoringSep` checks if 2 paths are the same regardless of their separators
 `"c:\temp\hello" must beEqualToIgnoringSep("c:/temp/hello")`
 * `beAnExistingPath` checks if a path exists
 * `beAReadablePath` checks if a path is readable
 * `beAWritablePath` checks if a path is writable
 * `beAnAbsolutePath` checks if a path is absolute
 * `beAHiddenPath` checks if a path is hidden
 * `beAFilePath` checks if a path is a file
 * `beADirectoryPath` checks if a path is a directory
 * `havePathName` checks if a path has a given name
 * `haveAsAbsolutePath` checks if a path has a given absolute path
 * `haveAsCanonicalPath` checks if a path has a given canonical path
 * `haveParentPath` checks if a path has a given parent path
 * `listPaths` checks if a path has a given list of children
 * `exist` checks if a file exists

 * `beReadable` checks if a file is readable
 * `beWritable` checks if a file is writable
 * `beAbsolute` checks if a file is absolute
 * `beHidden` checks if a file is hidden
 * `beAFile` checks if a file is a file
 * `beADirectory` checks if a file is a directory
 * `haveName` checks if a file has a given name
 * `haveAbsolutePath` checks if a file has a given absolute path
 * `haveCanonicalPath` checks if afile has a given canonical path
 * `haveParent` checks if a file has a given parent path
 * `haveList` checks if a file has a given list of children
"""
}
