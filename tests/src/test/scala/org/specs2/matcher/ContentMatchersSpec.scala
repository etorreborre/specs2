package org.specs2
package matcher

import io._
import specification.BeforeAfterEach
import control._
import text.LinesContent
import java.io.File
import ActionT._
import scalaz.std.anyVal._
import scalaz.syntax.bind._

class ContentMatchersSpec extends Spec with LinesContentMatchers with BeforeAfterEach with FileSystem with TestFileNames { def is = sequential ^ s2"""

 haveSameLinesAs checks if a file has the same lines as another file                                     ${comp().e1}
   it is possible to write (f1, f2) must haveSameLines as well                                           ${comp().e2}
   the comparison can be unordered                                                                       ${comp().e3}

 containLines checks if a file has the contains the lines of another file                                ${comp().e4}
   the comparison can be unordered                                                                       ${comp().e5}
   we can show only a given number of differences                                                        ${comp().e6}
   we can compare against a Seq of lines instead                                                         ${comp().e7}
   it works with duplicated lines                                                                        ${comp().e8}
                                                                                                         """
      
  lazy val dir = "target" / "test" / "contents"

  def before = {
    val action =
      writeFile(dir | f1, "hello\nbeautiful\nworld")         >>
      writeFile(dir | f2, "hello\nbeautiful\nworld")         >>
      writeFile(dir | f3, "beautiful\nworld\nhello")         >>
      writeFile(dir | f4, "hello\nworld")                    >>
      writeFile(dir | f5, "world\nhello")                    >>
      writeFile(dir | f6, "good\nmorning\nbeautiful\nworld") >>
      writeFile(dir | f7, "good\nday\ncrazy\nworld")         >>
      writeFile(dir | f8, "good\nday\ncrazy\nworld\nworld")

    action.execute(noLogging).unsafePerformIO
  }

  def after = delete(dir).execute(noLogging).unsafePerformIO

}

case class comp() extends MustMatchers with TestFileNames with ContentMatchers with FileSystem {
  lazy val dir = "target" / "test" / "contents"

  override implicit protected val fileContentForMatchers = new LinesContent[File] {
    def name(f: File) = f.getPath
    def lines(f: File) = readLines(FilePath.unsafe(f)).execute(noLogging).unsafePerformIO.toOption.get
  }

  def e1 =  (dir | f1).toFile must haveSameLinesAs((dir | f2).toFile)
  def e2 = ((dir | f1).toFile, (dir | f2).toFile) must haveSameLines
  def e3 = ((dir | f1).toFile, (dir | f2).toFile) must haveSameLines.unordered

  def e4 = (dir | f1).toFile must containLines((dir | f4).toFile)
  def e5 = (dir | f1).toFile must containLines((dir | f5).toFile).unordered

  def e6 = (((dir | f6).toFile, (dir | f7).toFile) must haveSameLines.showOnly(1.difference).unordered).message.split("\n").toSeq must
              haveSameLinesAs(Seq(
                s"${(dir | f6).path} is not the same as ${(dir | f7).path}",
                s"  in ${(dir | f6).path}, not in ${(dir | f7).path}",
                s"    2. morning",
                s"",
                s"  in ${(dir | f7).path}, not in ${(dir | f6).path}",
                s"    2. day"))

  def e7 = ((dir | f1).toFile, Seq("hello", "beautiful", "world")) must haveSameLines

  def e8 = ((dir | f8).toFile, (dir | f8).toFile) must haveSameLines

}

trait TestFileNames {
  lazy val f1 :: f2 :: f3 :: f4 :: f5 :: f6 :: f7 :: f8 :: _ =
    (1 to 8).toList.map(i => FileName.unsafe("f"+i))

}