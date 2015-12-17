package org.specs2
package matcher

import java.io.File
import execute.AsResult
import io._
import specification.BeforeAfterEach
import control._
import scalaz.syntax.bind._

class FilesContentMatchersSpec extends Spec
  with FilesContentMatchers with BeforeAfterEach with ThrownExpectations with FileSystem { def is = sequential ^ diffs(show = true, triggerSize = 0, diffRatio = 100)^ s2"""

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

  val (actual, expected, expected2, sub, f1, f2, f3) =
    (FileName.unsafe("actual"), FileName.unsafe("expected"), FileName.unsafe("expected2"),
      FileName.unsafe("sub"), FileName.unsafe("f1"), FileName.unsafe("f2"), FileName.unsafe("f3"))

  def e1 = {
    val action =
      createFile(targetDir / actual    | f1)       >>
      createFile(targetDir / actual    / sub | f2) >>
      createFile(targetDir / expected  | f1)       >>
      createFile(targetDir / expected  / sub | f2) >>
      createFile(targetDir / expected2 | f1)       >>
      createFile(targetDir / expected2 / sub | f3)

    action.runOption

    (targetDir / actual).toFile must haveSamePathsAs((targetDir / expected).toFile)

    ((targetDir / actual).toFile must haveSamePathsAs((targetDir / "expected2").toFile)) returns
     s"""|${(targetDir / actual).path} is not the same as ${(targetDir / expected2).path}
         |  in ${(targetDir / actual).path}, not in ${(targetDir / expected2).path}
         |    MISSING:   2. sub/f2
         |
         |  in ${(targetDir / expected2).path}, not in ${(targetDir / actual).path}
         |    MISSING:   2. sub/f3""".stripMargin
  }

  def e2 = {

    val action =
      createFile(targetDir / actual    | f1)         >>
      createFile(targetDir / actual    / sub | f2) >>
      createFile(targetDir / expected2 | f1)         >>
      createFile(targetDir / expected2 / sub | f2) >>
      createFile(targetDir / expected2 / sub | f3)

    action.runOption

    val notF3 = (f: File) => !f.getPath.endsWith("f3")

    (targetDir / actual).toFile must haveSamePathsAs((targetDir / expected2).toFile).withFilter(notF3)
  }

  def e3 = {
    val action =
      writeFile(targetDir / actual    | f1,         "text1")        >>
      writeFile(targetDir / actual    / sub | f2, "text2\ntext3") >>
      writeFile(targetDir / expected  | f1,         "text1")        >>
      writeFile(targetDir / expected  / sub | f2, "text2\ntext3") >>
      writeFile(targetDir / expected2 | f1,         "text1")        >>
      writeFile(targetDir / expected2 / sub | f2, "text2\ntext4")

    action.runOption

    (targetDir / actual).toFile must haveSameFilesContentAs((targetDir / expected).toFile)
    ((targetDir / actual).toFile must haveSameFilesContentAs((targetDir / expected2).toFile)) returns
     s"""|${(targetDir / actual / sub / f2).path} is not the same as ${(targetDir / expected2 / sub / f2).path}
         |  in ${(targetDir / actual / sub / f2).path}, not in ${(targetDir / expected2 / sub / f2).path}
         |    MISSING:   2. text3
         |
         |  in ${(targetDir / expected2 / sub / f2).path}, not in ${(targetDir / actual / sub / f2).path}
         |    MISSING:   2. text4""".stripMargin
  }

  def e4 = {
    val action =
      writeFile(targetDir / actual    | f1,          "text1")        >>
      writeFile(targetDir / actual    / sub | f2,  "text2\ntext3") >>
      writeFile(targetDir / expected  | f1,          "text1")        >>
      writeFile(targetDir / expected  / sub | f2,  "text2\ntext3") >>
      writeFile(targetDir / expected2 | f1,          "text1")        >>
      writeFile(targetDir / expected2 / sub | f2,  "text2\ntext4")

    action.runOption

    (targetDir | actual).toFile must haveSameFilesContentAs((targetDir | expected).toFile).withMatcher(haveSameMD5)

    AsResult((targetDir | actual).toFile must haveSameFilesContentAs((targetDir | expected2).toFile).withMatcher(haveSameMD5)).message.replace(" ", "") ===
      s"""|There is 1 failure
          |MD5 mismatch:
          |file                        | MD5
          |${(targetDir / actual / sub | f2).path}    | 4392ebd49e53e2cfe36abb22e39601db
          |${(targetDir / expected2 / sub | f2).path} | 1b7b2f1969fee054225ad6bbf7f6bdd7
          |""".stripMargin.replace(" ", "")
  }

  val targetDir = "target" / "test" / FileName.unsafe("fcm-"+hashCode)

  def before = FileSystem.mkdirs(targetDir).runOption
  def after  = FileSystem.delete(targetDir).runOption

}

