package org.specs2
package matcher
import java.io.File

class FileMatchersSpec extends SpecificationWithJUnit with TestFileSystem { def is = 
                                                                                          """
  The FileMatchers trait provides matchers to check files and paths.
  In the following, okPath references a path which actually exists and koPath, a path
  which doesn't.
                                                                                          """^
                                                                                          p^
  "The PathMatchers trait provide matchers for paths"                                     ^
    "beEqualToIgnoringSep checks if 2 paths are the same regardless of"                   ^bt^
    "their separators"                                                                    ^
    { "c:\\temp\\hello" must beEqualToIgnoringSep("c:/temp/hello") }                      ^
                                                                                          p^
    "beAnExistingPath checks if a path exists"                                            ^
    { okPath must be anExistingPath }                                                     ^
                                                                                          p^
                                                                                          end

}


//class fileMatchersSpec extends MatchersSpecification with TestFileSystem  {
//
//  "The PathMatchers trait" should {
//    "provide an beEqualToIgnoringSep matcher checking if two paths are the same regardless of their separators" in {
//      "c:\\temp\\hello" must beEqualToIgnoringSep("c:/temp/hello")
//      expectation("c:\\temp\\hello" must beEqualToIgnoringSep("c:/temp2/hello")) must failWith("'c:\\temp\\hello' is not equal ignoring separators to 'c:/temp2/hello'")
//    }
//    "provide an existPath / beAnExistingPath matcher to check if a file exists" in {
//      okPath must existPath
//      okPath must beAnExistingPath
//      expectation(missingPath must beAnExistingPath) must failWith("'absent' doesn't exist")
//      expectation(missingPath aka "missing path" must beAnExistingPath) must failWith("missing path 'absent' doesn't exist")
//    }
//    "provide an beAReadablePath matcher to check if a file can be read" in {
//      setReadable(okPath)
//      okPath must beAReadablePath
//
//      setNotReadable(okPath)
//      expectation(okPath must beAReadablePath) must failWith("'path' can't be read")
//      expectation(okPath aka "existing path" must beAReadablePath) must failWith("existing path 'path' can't be read")
//    }
//    "provide an beAWritablePath matcher to check if a file can be written" in {
//      setWritable(okPath)
//      okPath must beAWritablePath
//
//      setNotWritable(okPath)
//      expectation(okPath must beAWritablePath) must failWith("'path' can't be written")
//      expectation(okPath aka "existing path" must beAWritablePath) must failWith("existing path 'path' can't be written")
//    }
//    "provide an beAnAbsolutePath matcher to check if a file is absolute" in {
//      "/tmp" must beAnAbsolutePath
//      expectation(okPath must beAnAbsolutePath) must failWith("'path' is not absolute")
//      expectation(okPath aka "existing path" must beAnAbsolutePath) must failWith("existing path 'path' is not absolute")
//    }
//    "provide an beAHiddenPath matcher to check if a file is hidden" in {
//      ".tmp" must beAHiddenPath
//      expectation(okPath must beAHiddenPath) must failWith("'path' is not hidden")
//      expectation(okPath aka "existing path" must beAHiddenPath) must failWith("existing path 'path' is not hidden")
//    }
//    "provide an beAFilePath matcher to check if a file is a file" in {
//      "c:/tmp.txt" must beAFilePath
//      expectation("tmp/" must beAFilePath) must failWith("'tmp/' is not a file")
//      expectation("tmp/" aka "tmp path" must beAFilePath) must failWith("tmp path 'tmp/' is not a file")
//    }
//    "provide an beADirectoryPath matcher to check if a file is a directory" in {
//      "c:/tmp/" must beADirectoryPath
//      expectation("test.txt" must beADirectoryPath) must failWith("'test.txt' is not a directory")
//      expectation("test.txt" aka "this file" must beADirectoryPath) must failWith("this file 'test.txt' is not a directory")
//    }
//    "provide an havePathName matcher to check if a file has a given name" in {
//      "c:/tmp/test.txt" must havePathName("test.txt")
//      expectation("c:/tmp/test.txt" must havePathName("tst.txt")) must failWith("'c:/tmp/test.txt' is not named 'tst.txt'")
//      expectation("c:/tmp/test.txt" aka "the file" must havePathName("tst.txt")) must failWith("the file 'c:/tmp/test.txt' is not named 'tst.txt'")
//    }
//    "provide an haveAsAbsolutePath matcher to check if a file has a given absolute path" in {
//      "c:/tmp/test.txt" must haveAsAbsolutePath("c:/tmp/test.txt")
//      expectation("c:/tmp/test.txt" must haveAsAbsolutePath("tst.txt")) must failWithMatch("'c:/tmp/test.txt' doesn't have absolute path 'tst.txt' but .*")
//      expectation("c:/tmp/test.txt" aka "the file" must haveAsAbsolutePath("tst.txt")) must failWithMatch("the file 'c:/tmp/test.txt' doesn't have absolute path 'tst.txt' but .*")
//    }
//    "provide an haveAsCanonicalPath matcher to check if a file has a given canonical path" in {
//      "c:/tmp/dir/../test.txt" must haveAsCanonicalPath("c:/tmp/test.txt")
//      expectation("c:/tmp/dir/test.txt" must haveAsCanonicalPath("c:/tmp/test.txt")) must failWithMatch("'c:/tmp/dir/test.txt' doesn't have canonical path 'c:/tmp/test.txt' but .*")
//      expectation("c:/tmp/dir/test.txt" aka "the file" must haveAsCanonicalPath("c:/tmp/test.txt")) must failWithMatch("the file 'c:/tmp/dir/test.txt' doesn't have canonical path 'c:/tmp/test.txt' but .*")
//    }
//    "provide an haveParentPath matcher to check if a file has a given parent path" in {
//      "c:/tmp/dir/test.txt" must haveParentPath("c:/tmp/dir")
//      expectation("c:/tmp/dir/test.txt" must haveParentPath("c:/tmp/test.txt")) must failWithMatch("'c:/tmp/dir/test.txt' doesn't have parent path 'c:/tmp/test.txt' but .*")
//      expectation("c:/tmp/dir/test.txt" aka "the file" must haveParentPath("c:/tmp/test.txt")) must failWithMatch("the file 'c:/tmp/dir/test.txt' doesn't have parent path 'c:/tmp/test.txt' but .*")
//    }
//    "provide an listPaths matcher to check if a file has a given children" in {
//      addChild("c:/tmp", "c:/tmp/test.txt")
//      "c:/tmp" must listPaths("c:/tmp/test.txt")
//      expectation("c:/tmp" must listPaths("c:/tmp2/test.txt")) must failWith("'c:/tmp' doesn't have files 'c:/tmp2/test.txt' but 'c:/tmp/test.txt'")
//      expectation("c:/tmp" aka "the dir" must listPaths("c:/tmp2/test.txt")) must failWith("the dir 'c:/tmp' doesn't have files 'c:/tmp2/test.txt' but 'c:/tmp/test.txt'")
//    }
//
//    "provide an be equalToIgnoringSep matcher" in {
//      "c:\\temp\\hello" must be equalToIgnoringSep("c:/temp/hello")
//    }
//    "provide an be an existingPath matcher to check if a file exists" in {
//      okPath must be an existingPath
//    }
//    "provide an be a readablePath matcher to check if a file can be read" in {
//      setReadable(okPath)
//      okPath must be a readablePath
//    }
//    "provide an be a writablePath matcher to check if a file can be written" in {
//      setWritable(okPath)
//      okPath must be a writablePath
//    }
//    "provide an be an absolutePath matcher to check if a file is absolute" in {
//      "/tmp" must be an absolutePath
//    }
//    "provide an be a hiddenPath matcher to check if a file is hidden" in {
//      ".tmp" must be a hiddenPath
//    }
//    "provide an be a filePath matcher to check if a file is a file" in {
//      "c:/tmp.txt" must be a filePath
//    }
//    "provide an be a directoryPath matcher to check if a file is a directory" in {
//      "c:/tmp/" must be a directoryPath
//    }
//    "provide an have pathName matcher to check if a file has a given name" in {
//      "c:/tmp/test.txt" must have pathName("test.txt")
//    }
//    "provide an have asAbsolutePath matcher to check if a file has a given absolute path" in {
//      "c:/tmp/test.txt" must have asAbsolutePath("c:/tmp/test.txt")
//    }
//    "provide an have asCanonicalPath matcher to check if a file has a given canonical path" in {
//      "c:/tmp/dir/../test.txt" must have asCanonicalPath("c:/tmp/test.txt")
//    }
//    "provide an have ParentPath matcher to check if a file has a given parent path" in {
//      "c:/tmp/dir/test.txt" must have parentPath("c:/tmp/dir")
//    }
//  }
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
