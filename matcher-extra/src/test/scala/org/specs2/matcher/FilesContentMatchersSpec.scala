package org.specs2
package matcher

import specification.BeforeAfterExample
import java.io.File
import execute.AsResult

class FilesContentMatchersSpec extends Specification
  with FilesContentMatchers with BeforeAfterExample with ThrownExpectations { def is = sequential ^ diffs(show = true, triggerSize = 0, diffRatio = 100)^ s2"""

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
    createFile("test/actual/f1")
    createFile("test/actual/sub/f2")

    createFile("test/expected/f1")
    createFile("test/expected/sub/f2")

    createFile("test/expected2/f1")
    createFile("test/expected2/sub/f3")

    "test/actual".file must haveSamePathsAs("test/expected".file)
    ("test/actual".file must haveSamePathsAs("test/expected2".file)) returns
      """|test/actual is not the same as test/expected2
         |  in test/actual, not in test/expected2
         |    MISSING:   3. sub/f2
         |
         |  in test/expected2, not in test/actual
         |    MISSING:   3. sub/f3
         |""".stripMargin
  }

  def e2 = {
    createFile("test/actual/f1")
    createFile("test/actual/sub/f2")

    createFile("test/expected/f1")
    createFile("test/expected/sub/f2")
    createFile("test/expected/sub/f3")
    val notF3 = (f: File) => !f.getPath.endsWith("f3")

    "test/actual".file must haveSamePathsAs("test/expected".file).withFilter(notF3)
  }

  def e3 = {
    writeFile("test/actual/f1", "text1")
    writeFile("test/actual/sub/f2", "text2\ntext3")
    
    writeFile("test/expected/f1", "text1")
    writeFile("test/expected/sub/f2", "text2\ntext3")
    
    writeFile("test/expected2/f1", "text1")
    writeFile("test/expected2/sub/f2", "text2\ntext4")

    "test/actual".file must haveSameFilesContentAs("test/expected".file)
    ("test/actual".file must haveSameFilesContentAs("test/expected2".file)) returns
      """|test/actual/sub/f2 is not the same as test/expected2/sub/f2
         |  in test/actual/sub/f2, not in test/expected2/sub/f2
         |    MISSING:   2. text3
         |
         |  in test/expected2/sub/f2, not in test/actual/sub/f2
         |    MISSING:   2. text4
         |""".stripMargin
  }

  def e4 = {
    writeFile("test/actual/f1", "text1")
    writeFile("test/actual/sub/f2", "text2\ntext3")

    writeFile("test/expected/f1", "text1")
    writeFile("test/expected/sub/f2", "text2\ntext3")

    writeFile("test/expected2/f1", "text1")
    writeFile("test/expected2/sub/f2", "text2\ntext4")

    "test/actual".file must haveSameFilesContentAs("test/expected".file).withMatcher(haveSameMD5)
    AsResult("test/actual".file must haveSameFilesContentAs("test/expected2".file).withMatcher(haveSameMD5)).message ===
      // be careful with whitespace after 'MD5' !!
      """|There is 1 failure
         |MD5 mismatch:
         |file                  | MD5                             
         |test/actual/sub/f2    | 4392ebd49e53e2cfe36abb22e39601db
         |test/expected2/sub/f2 | 1b7b2f1969fee054225ad6bbf7f6bdd7
         |""".stripMargin
  }

  def before = new File("test").mkdir
  def after = removeDir("test")

  implicit class pathToFile(s: String) {
    def file = new File(s)
  }
}
