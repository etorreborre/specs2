package org.specs2
package matcher

import io.MockFileSystem
import text.LinesContent
import java.io.File

class ContentMatchersSpec extends Specification { def is =

  "haveSameLinesAs checks if a file has the same lines as another file"                                                 ! comp().e1^
    "it is possible to write (f1, f2) must haveSameLines as well"                                                       ! comp().e2^
    "the comparison can be unordered"                                                                                   ! comp().e3^
                                                                                                                        endp^
  "containLines checks if a file has the contains the lines of another file"                                            ! comp().e4^
    "the comparison can be unordered"                                                                                   ! comp().e5^
    "we can show only a given number of differences"                                                                    ! comp().e6^
    "we can compare against a Seq of lines instead"                                                                     ! comp().e7^
                                                                                                                        end
  def e8 = (new File("f1.txt"), new File("f2.txt")) must haveSameLines.unordered

}

case class comp() extends MustMatchers with MockFileSystem with TestFiles {

  addFile("f1", "hello\nbeautiful\nworld")
  addFile("f2", "hello\nbeautiful\nworld")
  addFile("f3", "beautiful\nworld\nhello")
  addFile("f4", "hello\nworld")
  addFile("f5", "world\nhello")
  addFile("f6", "good\nmorning\nbeautiful\nworld")
  addFile("f7", "good\nday\ncrazy\nworld")

  // read contents from the mock file system
  override implicit protected val fileContentForMatchers = new LinesContent[File] {
    def name(f: File) = f.getPath
    def lines(f: File) = readLines(f.getPath)
  }


  def e1 = file("f1") must haveSameLinesAs(file("f2"))
  def e2 = (file("f1"), file("f2")) must haveSameLines
  def e3 = (file("f1"), file("f2")) must haveSameLines.unordered

  def e4 = file("f1") must containLines(file("f4"))
  def e5 = file("f1") must containLines(file("f5")).unordered

  def e6 = ((file("f6"), file("f7")) must haveSameLines.showOnly(1.difference).unordered).message.split("\n").toSeq must
              haveSameLinesAs(Seq(
                "f6 is not the same as f7",
                "in f6, not in f7",
                "  2. morning",
                "in f7, not in f6",
                "  2. day"))

  def e7 = (file("f1"), Seq("hello", "beautiful", "world")) must haveSameLines

}

