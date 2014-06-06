package org.specs2
package matcher

import java.io.File
import execute.AsResult
import io._
import specification.BeforeAfterExample
import control._
import Actions._
import scalaz.std.anyVal._
import scalaz.syntax.bind._

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
    val action =
      createFile(s"$targetDir/actual/f1")        >>
      createFile(s"$targetDir/actual/sub/f2")    >>
      createFile(s"$targetDir/expected/f1")      >>
      createFile(s"$targetDir/expected/sub/f2")  >>
      createFile(s"$targetDir/expected2/f1")     >>
      createFile(s"$targetDir/expected2/sub/f3")

    action.execute(noLogging).unsafePerformIO

     s"$targetDir/actual".file must haveSamePathsAs(s"$targetDir/expected".file)
    (s"$targetDir/actual".file must haveSamePathsAs(s"$targetDir/expected2".file)) returns
     s"""|$targetDir/actual is not the same as $targetDir/expected2
         |  in $targetDir/actual, not in $targetDir/expected2
         |    MISSING:   3. sub/f2
         |
         |  in $targetDir/expected2, not in $targetDir/actual
         |    MISSING:   3. sub/f3""".stripMargin
  }

  def e2 = {

    val action =
      createFile(s"$targetDir/actual/f1")        >>
      createFile(s"$targetDir/actual/sub/f2")    >>
      createFile(s"$targetDir/expected/f1")      >>
      createFile(s"$targetDir/expected/sub/f2")  >>
      createFile(s"$targetDir/expected/sub/f3")

    action.execute(noLogging).unsafePerformIO

    val notF3 = (f: File) => !f.getPath.endsWith("f3")

    s"$targetDir/actual".file must haveSamePathsAs(s"$targetDir/expected".file).withFilter(notF3)
  }

  def e3 = {
    val action =
      writeFile(s"$targetDir/actual/f1", "text1")               >>
      writeFile(s"$targetDir/actual/sub/f2", "text2\ntext3")    >>
      writeFile(s"$targetDir/expected/f1", "text1")             >>
      writeFile(s"$targetDir/expected/sub/f2", "text2\ntext3")  >>
      writeFile(s"$targetDir/expected2/f1", "text1")            >>
      writeFile(s"$targetDir/expected2/sub/f2", "text2\ntext4")

    action.execute(noLogging).unsafePerformIO

    s"$targetDir/actual".file must haveSameFilesContentAs(s"$targetDir/expected".file)
    (s"$targetDir/actual".file must haveSameFilesContentAs(s"$targetDir/expected2".file)) returns
     s"""|$targetDir/actual/sub/f2 is not the same as $targetDir/expected2/sub/f2
         |  in $targetDir/actual/sub/f2, not in $targetDir/expected2/sub/f2
         |    MISSING:   2. text3
         |
         |  in $targetDir/expected2/sub/f2, not in $targetDir/actual/sub/f2
         |    MISSING:   2. text4""".stripMargin
  }

  def e4 = {
    val action =
      writeFile(s"$targetDir/actual/f1", "text1")               >>
      writeFile(s"$targetDir/actual/sub/f2", "text2\ntext3")    >>
      writeFile(s"$targetDir/expected/f1", "text1")             >>
      writeFile(s"$targetDir/expected/sub/f2", "text2\ntext3")  >>
      writeFile(s"$targetDir/expected2/f1", "text1")            >>
      writeFile(s"$targetDir/expected2/sub/f2", "text2\ntext4")

    action.execute(noLogging).unsafePerformIO

    s"$targetDir/actual".file must haveSameFilesContentAs(s"$targetDir/expected".file).withMatcher(haveSameMD5)
    AsResult(s"$targetDir/actual".file must haveSameFilesContentAs(s"$targetDir/expected2".file).withMatcher(haveSameMD5)).message.replace(" ", "") ===
      s"""|There is 1 failure
          |MD5 mismatch:
          |file                        | MD5
          |$targetDir/actual/sub/f2    | 4392ebd49e53e2cfe36abb22e39601db
          |$targetDir/expected2/sub/f2 | 1b7b2f1969fee054225ad6bbf7f6bdd7
          |""".stripMargin.replace(" ", "")
  }

  val targetDir = "target/test/fcm"
  def before = new File(targetDir).mkdir
  def after = delete(targetDir).execute(noLogging).unsafePerformIO

  implicit class pathToFile(s: String) {
    def file = new File(s)
  }
}
