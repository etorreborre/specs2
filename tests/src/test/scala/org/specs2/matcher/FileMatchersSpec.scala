package org.specs2
package matcher

import java.io.File
import io._
import execute.StandardResults
import org.specs2.specification._
import control._

class FileMatchersSpec extends Spec with TestFiles with FileMatchers {  def is = sequential ^ s2"""

 The FileMatchers trait provides matchers to check files and paths.

 In the following, okPath references a path which actually exists and missingPath, a path which doesn't.


 The PathMatchers trait provides matchers for paths
   beEqualToIgnoringSep checks if 2 paths are the same regardless of
   their separators
   ${ "c:\\temp\\hello" must beEqualToIgnoringSep("c:/temp/hello") }

   beAnExistingPath checks if a path exists
     `okPath must beAnExistingPath`                                                                      ${fs().e1}
     `missingPath must not be anExistingPath`                                                            ${fs().e2}

   beAReadablePath checks if a path is readable
      `setReadable(okPath) must beAReadablePath`                                                         ${fs().e3}
      `setNotReadable(okPath) must not be aReadablePath`                                                 ${fs().e4}

   beAWritablePath checks if a path is writable
     `setWritable(okPath) must beAWritablePath`                                                          ${fs().e5}
     `setNotWritable(okPath) must not be aWritablePath`                                                  ${fs().e6}

   beAnAbsolutePath checks if a path is absolute
   ${ "/tmp" must beAnAbsolutePath }
   ${ "./tmp" must not be anAbsolutePath }

   "beAHiddenPath checks if a path is hidden
   ${ ".tmp" must beAHiddenPath }
   ${ "/tmp" must not be aHiddenPath }

   beAFilePath checks if a path is a file
   ${ okPath must beAFilePath }
   ${ dirPath must not be aFilePath }

   beADirectorPath checks if a path is a directory
   ${ dirPath must beADirectoryPath }
   ${ okPath must not be aDirectoryPath }

   havePathName checks if a path has a given name
   ${ okPath must havePathName("file.txt") }
   ${ okPath must not have pathName("name.txt") }

   haveAsAbsolutePath checks if a path has a given absolute path
   ${ okPath must haveAsAbsolutePath(new File(okPath).getAbsolutePath) }

   haveAsCanonicalPath checks if a path has a given canonical path
   ${ "c:/tmp/../dir" must haveAsCanonicalPath("c:/dir") }

   haveParentPath checks if a path has a given parent path
   ${ okPath must haveParentPath(dirPath) }

   listPaths checks if a path has a given list of children                                               ${fs().e7}

 The FileMatchers trait provides similar matchers, but for files
   exist checks if a file exists                                                                         ${fs().e8}
   beReadable checks if a file is readable                                                               ${fs().e9}
   beWritable checks if a file is writable                                                               ${fs().e10}

   beAbsolute checks if a file is absolute
   ${ file("/tmp") must beAbsolute }

   beHidden checks if a file is hidden
   ${ file(".tmp") must beHidden }

   beAFile checks if a file is a file
   ${ file(okPath) must beAFile }

   beADirectory checks if a file is a directory
   ${ file(dirPath) must beADirectory }

   haveName checks if a file has a given name
   ${ file(okPath) must haveName(new File(okPath).getName) }

   haveAbsolutePath checks if a file has a given absolute path
   ${ file(okPath) must haveAbsolutePath(new File(okPath).getAbsolutePath) }

   haveCanonicalPath checks if a file has a given canonical path
   ${ file("c:/tmp/../dir/test.txt") must haveCanonicalPath("c:/dir/test.txt") }

   haveParent checks if a file has a given parent path
   ${ file("c:/tmp/dir/test.txt") must haveParent("c:/tmp/dir") }

   haveList checks if a file has a given list of children ${fs().e11}
                                                                                                                        """

}

case class fs() extends MustMatchers with TestFiles with FileMatchers with StandardResults with Debug {
  def e1 = okPath must beAnExistingPath
  def e2 = missingPath must not be anExistingPath
  def e3 = setReadable(okPath, true) must beAReadablePath
  def e4 = setReadable(okPath, false) must not be aReadablePath
  def e5 = setWritable(okPath, true) must beAWritablePath
  def e6 = setWritable(okPath, false) must not be aWritablePath
  def e7 = dirPath must listPaths("file.txt")
  def e8 = file(okPath) must exist
  def e9 =  file(setReadable(okPath, true)) must beReadable
  def e10 = file(setWritable(okPath, true)) must beWritable
  def e11 = file(dirPath) must haveList("file.txt")

}


trait TestFiles extends FileSystem with BeforeAfterEach {
  lazy val directoryPath = "target" / "test" / "fs"
  lazy val dirPath = directoryPath.path
  lazy val okFilePath = directoryPath | "file.txt"
  lazy val okPath = okFilePath.path
  lazy val missingPath = "absent"

  def before =
    writeFile(okFilePath, "").execute(noLogging).unsafePerformIO

  def after = delete(directoryPath).execute(noLogging).unsafePerformIO

  def setReadable(path: String, r: Boolean) = {
    new File(path).setReadable(r)
    path
  }

  def setWritable(path: String, r: Boolean) = {
    new File(path).setWritable(r)
    path
  }

  def file(s: String) = new File(s)
}