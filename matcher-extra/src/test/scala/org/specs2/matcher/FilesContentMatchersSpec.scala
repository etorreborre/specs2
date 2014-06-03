package org.specs2
package matcher

import java.io.File
import execute.AsResult
import io._
import org.specs2.specification.BeforeAfterExample

class FilesContentMatchersSpec extends Specification
  with FilesContentMatchers with BeforeAfterExample with ThrownExpectations with FileSystem { def is = sequential ^ diffs(show = true, triggerSize = 0, diffRatio = 100)^ s2"""

 File content matchers help to compare the contents of 2 directories:

  - to check if the directories contain the same files
  - to check if the file contents are the same

### Checking directories

 It is possible to check if the paths of 2 directories are the same $e1
 It is possible to exclude some paths from the check $e2

### Checking file contents

 It is possible to check if the files contained in 2 directories
   contain the same lines $e3
   have the same MD5 hash $e4
                          """

  def e1 = {
    createFile("target/test/actual/f1")
    createFile("target/test/actual/sub/f2")

    createFile("target/test/expected/f1")
    createFile("target/test/expected/sub/f2")

    createFile("target/test/expected2/f1")
    createFile("target/test/expected2/sub/f3")

     "target/test/actual".file must haveSamePathsAs("target/test/expected".file)
    ("target/test/actual".file must haveSamePathsAs("target/test/expected2".file)) returns
      """|target/test/actual is not the same as target/test/expected2
         |  in target/test/actual, not in target/test/expected2
         |    MISSING:   3. sub/f2
         |
         |  in target/test/expected2, not in target/test/actual
         |    MISSING:   3. sub/f3
         |""".stripMargin
  }

  def e2 = {
    createFile("target/test/actual/f1")
    createFile("target/test/actual/sub/f2")

    createFile("target/test/expected/f1")
    createFile("target/test/expected/sub/f2")
    createFile("target/test/expected/sub/f3")
    val notF3 = (f: File) => !f.getPath.endsWith("f3")

    "target/test/actual".file must haveSamePathsAs("target/test/expected".file).withFilter(notF3)
  }

  def e3 = {
    writeFile("target/test/actual/f1", "text1")
    writeFile("target/test/actual/sub/f2", "text2\ntext3")
    
    writeFile("target/test/expected/f1", "text1")
    writeFile("target/test/expected/sub/f2", "text2\ntext3")
    
    writeFile("target/test/expected2/f1", "text1")
    writeFile("target/test/expected2/sub/f2", "text2\ntext4")

    "target/test/actual".file must haveSameFilesContentAs("target/test/expected".file)
    ("target/test/actual".file must haveSameFilesContentAs("target/test/expected2".file)) returns
      """|target/test/actual/sub/f2 is not the same as target/test/expected2/sub/f2
         |  in target/test/actual/sub/f2, not in target/test/expected2/sub/f2
         |    MISSING:   2. text3
         |
         |  in target/test/expected2/sub/f2, not in target/test/actual/sub/f2
         |    MISSING:   2. text4
         |""".stripMargin
  }

  def e4 = {
    writeFile("target/test/actual/f1", "text1")
    writeFile("target/test/actual/sub/f2", "text2\ntext3")

    writeFile("target/test/expected/f1", "text1")
    writeFile("target/test/expected/sub/f2", "text2\ntext3")

    writeFile("target/test/expected2/f1", "text1")
    writeFile("target/test/expected2/sub/f2", "text2\ntext4")

    "target/test/actual".file must haveSameFilesContentAs("target/test/expected".file).withMatcher(haveSameMD5)
    AsResult("target/test/actual".file must haveSameFilesContentAs("target/test/expected2".file).withMatcher(haveSameMD5)).message ===
      // be careful with whitespace after 'MD5' !!
      """|There is 1 failure
         |MD5 mismatch:
         |file                         | MD5
         |target/test/actual/sub/f2    | 4392ebd49e53e2cfe36abb22e39601db
         |target/test/expected2/sub/f2 | 1b7b2f1969fee054225ad6bbf7f6bdd7
         |""".stripMargin
  }

  def before = new File("target/test").mkdir
  def after = delete("target/test")

  implicit class pathToFile(s: String) {
    def file = new File(s)
  }
}
