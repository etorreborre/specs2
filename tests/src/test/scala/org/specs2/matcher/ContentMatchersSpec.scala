package org.specs2
package matcher

import io._
import specification.BeforeAfterEach
import control._
import text.LinesContent
import java.io.File
import Actions._
import ActionT._
import scalaz.std.anyVal._
import scalaz.syntax.bind._

class ContentMatchersSpec extends Specification with LinesContentMatchers with BeforeAfterEach with FileSystem { def is = sequential ^ s2"""

 haveSameLinesAs checks if a file has the same lines as another file                                     ${comp().e1}
   it is possible to write (f1, f2) must haveSameLines as well                                           ${comp().e2}
   the comparison can be unordered                                                                       ${comp().e3}

 containLines checks if a file has the contains the lines of another file                                ${comp().e4}
   the comparison can be unordered                                                                       ${comp().e5}
   we can show only a given number of differences                                                        ${comp().e6}
   we can compare against a Seq of lines instead                                                         ${comp().e7}
   it works with duplicated lines                                                                        ${comp().e8}
                                                                                                         """
      
  lazy val dir = "target/test/contents"

  def before = {
    val action =
      writeFile(s"$dir/f1", "hello\nbeautiful\nworld")         >>
      writeFile(s"$dir/f2", "hello\nbeautiful\nworld")         >>
      writeFile(s"$dir/f3", "beautiful\nworld\nhello")         >>
      writeFile(s"$dir/f4", "hello\nworld")                    >>
      writeFile(s"$dir/f5", "world\nhello")                    >>
      writeFile(s"$dir/f6", "good\nmorning\nbeautiful\nworld") >>
      writeFile(s"$dir/f7", "good\nday\ncrazy\nworld")         >>
      writeFile(s"$dir/f8", "good\nday\ncrazy\nworld\nworld")

    action.execute(noLogging).unsafePerformIO
  }

  def after = delete(s"$dir").execute(noLogging).unsafePerformIO
}

case class comp() extends MustMatchers with TestFiles with ContentMatchers with FileSystem {
  lazy val dir = "target/test/contents"

  override implicit protected val fileContentForMatchers = new LinesContent[File] {
    def name(f: File) = f.getPath
    def lines(f: File) = readLines(f.getPath).execute(noLogging).unsafePerformIO.toOption.get
  }

  def e1 =  file(s"$dir/f1") must haveSameLinesAs(file(s"$dir/f2"))
  def e2 = (file(s"$dir/f1"), file(s"$dir/f2")) must haveSameLines
  def e3 = (file(s"$dir/f1"), file(s"$dir/f2")) must haveSameLines.unordered

  def e4 = file(s"$dir/f1") must containLines(file(s"$dir/f4"))
  def e5 = file(s"$dir/f1") must containLines(file(s"$dir/f5")).unordered

  def e6 = ((file(s"$dir/f6"), file(s"$dir/f7")) must haveSameLines.showOnly(1.difference).unordered).message.split("\n").toSeq must
              haveSameLinesAs(Seq(
                s"$dir/f6 is not the same as $dir/f7",
                s"  in $dir/f6, not in $dir/f7",
                s"    2. morning",
                s"",
                s"  in $dir/f7, not in $dir/f6",
                s"    2. day"))

  def e7 = (file(s"$dir/f1"), Seq("hello", "beautiful", "world")) must haveSameLines

  def e8 = (file(s"$dir/f8"), file(s"$dir/f8")) must haveSameLines

}

