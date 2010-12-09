package org.specs2
package matcher
import java.io.File

class FileMatchersSpec extends SpecificationWithJUnit with TestFileSystem { def is = 
                                                                                          """
  The FileMatchers trait provides matchers to check files and paths.
  In the following, okPath references a path which actually exists and missingPath, a path
  which doesn't.
  
                                                                                          """^
                                                                                          p^
  "The PathMatchers trait provide matchers for paths"                                     ^
    "beEqualToIgnoringSep checks if 2 paths are the same regardless of"                   ^bt^
    "their separators"                                                                    ^
    { "c:\\temp\\hello" must beEqualToIgnoringSep("c:/temp/hello") }                      ^
                                                                                          p^
    "beAnExistingPath checks if a path exists"                                            ^
    { okPath must beAnExistingPath }                                                      ^
    { missingPath must not be anExistingPath }                                            ^
                                                                                          p^
    "beAReadablePath checks if a path is readable"                                        ^
    { setReadable(okPath) must beAReadablePath }                                          ^
    { setNotReadable(okPath) must not be aReadablePath }                                  ^
                                                                                          p^
    "beAWritablePath checks if a path is writable"                                        ^
    { setWritable(okPath) must beAWritablePath }                                          ^
    { setNotWritable(okPath) must not be aWritablePath }                                  ^
                                                                                          p^
    "beAnAbsolutePath checks if a path is absolute"                                       ^
    { "/tmp" must beAnAbsolutePath }                                                      ^
    { "./tmp" must not be anAbsolutePath }                                                ^
                                                                                          p^
    "beAHiddenPath checks if a path is hidden"                                            ^
    { ".tmp" must beAHiddenPath }                                                         ^
    { "/tmp" must not be aHiddenPath }                                                    ^
                                                                                          p^
    "beAFilePath checks if a path is a file"                                              ^
    { "c:/tmp.txt" must beAFilePath }                                                     ^
    { "c:/tmp" must not be aFilePath }                                                    ^
                                                                                          p^
    "beADirectorPath checks if a path is a directory"                                     ^
    { "c:/tmp" must beADirectoryPath }                                                    ^
    { "c:/tmp.txt" must not be aDirectoryPath }                                           ^
                                                                                          p^
    "havePathName checks if a path has a given name"                                      ^
    { "c:/tmp/test.txt" must havePathName("test.txt") }                                   ^
    { "c:/tmp/test.txt" must not have pathName("name.txt") }                              ^
                                                                                          p^
    "haveAsAbsolutePath checks if a path has a given absolute path"                       ^
    { "c:/tmp/test.txt" must haveAsAbsolutePath("c:/tmp/test.txt") }                      ^
                                                                                          p^
    "haveAsCanonicalPath checks if a path has a given canonical path"                     ^
    { "c:/tmp/../test.txt" must haveAsCanonicalPath("c:/test.txt") }                      ^
                                                                                          p^
    "haveParentPath checks if a path has a given parent path"                             ^
    { "c:/tmp/dir/test.txt" must haveParentPath("c:/tmp/dir") }                           ^
                                                                                          p^
    "listPaths checks if a path has a given list of children"                             ^
    { addChild("c:/t/", "c:/t/test.txt"); "c:/t/" must listPaths("c:/t/test.txt") }       ^
                                                                                          p^
  "The FileMatchers trait provides similar matchers, but for files"                       ^
    "exist checks if a file exists"                                                       ^
    { file(okPath) must exist }                                                           ^
                                                                                          p^
    "beReadable checks if a file is readable"                                             ^
    { file(setReadable(okPath)) must beReadable }                                         ^
                                                                                          p^
    "beWritable checks if a file is writable"                                             ^
    { file(setWritable(okPath)) must beWritable }                                         ^
                                                                                          p^
    "beAbsolute checks if a file is absolute"                                             ^
    { file("/tmp") must beAbsolute }                                                      ^
                                                                                          p^
    "beHidden checks if a file is hidden"                                                 ^
    { file(".tmp") must beHidden }                                                        ^
                                                                                          p^
    "beAFile checks if a file is a file"                                                  ^
    { file("c:/tmp.txt") must beAFile }                                                   ^
                                                                                          p^
    "beADirectory checks if a file is a directory"                                         ^
    { file("c:/tmp") must beADirectory }                                                  ^
                                                                                          p^
    "haveName checks if a file has a given name"                                          ^
    { file("c:/tmp/test.txt") must haveName("test.txt") }                                 ^
                                                                                          p^
    "haveAbsolutePath checks if a file has a given absolute path"                         ^
    { file("c:/tmp/test.txt") must haveAbsolutePath("c:/tmp/test.txt") }                  ^
                                                                                          p^
    "haveCanonicalPath checks if afile has a given canonical path"                        ^
    { file("c:/tmp/dir/../test.txt") must haveCanonicalPath("c:/tmp/test.txt") }          ^
                                                                                          p^
    "haveParent checks if a file has a given parent path"                                 ^
    { file("c:/tmp/dir/test.txt") must haveParent("c:/tmp/dir") }                         ^
                                                                                          p^
    "haveList checks if a file has a given list of children"                              ^
    { addChild("c:/t", "c:/t/test.txt"); file("c:/t/") must haveList("c:/t/test.txt") }  ^
                                                                                          end
  def file(s: String) = new File(s)
}


//class fileMatchersSpec extends MatchersSpecification with TestFileSystem  {
//  "The File matchers" should { 
//    "provide an exist matcher to check if a file exists" in {
//      new File(okPath) must exist
//    }
//    "provide a beReadable matcher to check if a file can be read" in {
//      setReadable(okPath)
//      new File(okPath) must beReadable
//    }
//    "provide a beWritable matcher to check if a file can be written" in {
//      setWritable(okPath)
//      new File(okPath) must beWritable
//    }
//    "provide a beAbsolute matcher to check if a file is absolute" in {
//      new File("/tmp") must beAbsolute
//    }
//    "provide a beHiddenPath matcher to check if a file is hidden" in {
//      new File(".tmp") must beHidden
//    }
//    "provide a beFile matcher to check if a file is a file" in {
//      new File("c:/tmp.txt") must beFile
//    }
//    "provide a beDirectory matcher to check if a file is a directory" in {
//      new File("c:/tmp/") must beDirectory
//    }
//    "provide a haveName matcher to check if a file has a given name" in {
//      new File("c:/tmp/test.txt") must haveName("test.txt")
//    }
//    "provide a haveAbsolutePath matcher to check if a file has a given absolute path" in {
//      new File("c:/tmp/test.txt") must haveAbsolutePath("c:/tmp/test.txt")
//    }
//    "provide a haveCanonicalPath matcher to check if a file has a given canonical path" in {
//      new File("c:/tmp/dir/../test.txt") must haveCanonicalPath("c:/tmp/test.txt")
//    }
//    "provide a haveParent matcher to check if a file has a given parent path" in {
//      new File("c:/tmp/dir/test.txt") must haveParent("c:/tmp/dir")
//    }
//    "provide a haveList matcher to check if a file has a given children" in {
//      addChild("c:/tmp", "c:/tmp/test.txt")
//      new File("c:/tmp") must haveList("c:/tmp/test.txt")
//    }
//    "provide a be readable matcher to check if a file can be read" in {
//      setReadable(okPath)
//      new File(okPath) must be readable
//    }
//    "provide a be writable matcher to check if a file can be written" in {
//      setWritable(okPath)
//      new File(okPath) must be writable
//    }
//    "provide a be absolute matcher to check if a file is absolute" in {
//      new File("/tmp") must be absolute
//    }
//    "provide a be hiddenPath matcher to check if a file is hidden" in {
//      new File(".tmp") must be hidden
//    }
//    "provide a be a file matcher to check if a file is a file" in {
//      new File("c:/tmp.txt") must be a file
//    }
//    "provide a be a directory matcher to check if a file is a directory" in {
//      new File("c:/tmp/") must be a directory
//    }
//    "provide a have the name matcher to check if a file has a given name" in {
//      new File("c:/tmp/test.txt") must have the name("test.txt")
//    }
//    "provide a have the absolutePath matcher to check if a file has a given absolute path" in {
//      new File("c:/tmp/test.txt") must have the absolutePath("c:/tmp/test.txt")
//    }
//    "provide a have the canonicalPath matcher to check if a file has a given canonical path" in {
//      new File("c:/tmp/dir/../test.txt") must have the canonicalPath("c:/tmp/test.txt")
//    }
//    "provide a have the parent matcher to check if a file has a given parent path" in {
//      new File("c:/tmp/dir/test.txt") must have the parent("c:/tmp/dir")
//    }
//    "provide a have paths matcher to check if a file has a given children" in {
//      addChild("c:/tmp", "c:/tmp/test.txt")
//      new File("c:/tmp") must have paths("c:/tmp/test.txt")
//    }
//  }
//  "The File matchers" can {
//    "be used from string using the path function, like 'c:/projects'.path must exist" in {
//      okPath.path must exist
//    }
//  }
//}
import io._
trait TestFileSystem extends MockFileSystem {
  val okPath = "path"
  val missingPath = "absent"
  addFile(okPath, "")
}
