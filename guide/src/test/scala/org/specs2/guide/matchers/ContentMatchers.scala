package org.specs2
package guide
package matchers

import java.io.File
import text.LinesContent

object ContentMatchers extends UserGuideCard with matcher.ContentMatchers {
  def title = "Content"
  def text = s2"""

##### File contents

The matchers from the `org.specs2.matcher.ContentMatchers` trait can help us check the contents of files. For example we can check that 2 text files have the same lines: ${snippet{
(file1, file2) must haveSameLines
file1 must haveSameLinesAs(file2)
}}

We can check that the content of one file is contained in another one: ${snippet{

file1 must containLines(file2)

}}

If the files are binary files we can also check that they have the same MD5 hash: ${snippet{

 (file1, file2) must haveSameMD5
 file1 must haveSameMD5As(file2)

}}

***Order***

It is possible to relax the constraint by requiring the equality or containment to be true regardless of the order of lines: ${snippet{

 (file1, file2) must haveSameLines.unordered
 file1 must haveSameLinesAs(file2).unordered
 file1 must containLines(file2).unordered
}}

***Missing only***

By default, `(file1, file2) must haveSameLines` will report misplaced lines if any, that is, lines of `f1` which appear in `f2` but not at the right position. However if `file2` is big, this search might degrade the performances. In that case you can turn it off with `missingOnly`: ${snippet{

 (file1, file2) must haveSameLines.missingOnly

}}

***Show less differences***

If there are too many differences, you can specify that you only want the first 10: ${snippet{

 (file1, file2) must haveSameLines.showOnly(10.differences).unordered

}}

In the code above `10.differences` builds a `DifferenceFilter` which is merely a filtering function: `(lines1: Seq[String], lines2: Seq[String]) => (Seq[String], Seq[String])`. The parameter `lines1` is the sequence of lines not found in the second content while `lines2` is the sequence of lines not found in the first content.

##### Directories contents

We can compare the contents of 2 directories. We can for example check if no files are missing and none has been added: ${snippet{

actualDir must haveSamePathsAs(expectedDir)
// with a file filter applied to both the actual and expected directories
actualDir must haveSamePathsAs(expectedDir).withFilter((file: File) => !file.isHidden)

}}

Once we know that all files are present we can check their content: ${snippet{

// the default comparison expects that files are text files and that comparison must be done line by line
actualDir must haveSameFilesAs(expectedDir)

// with a file filter applied to both the actual and expected directories
actualDir must haveSameFilesAs(expectedDir).withFilter((file: File) => !file.isHidden)

// with a MD5 matcher for binary files
actualDir must haveSameFilesAs(expectedDir).withMatcher(haveSameMD5)

// it is also possible to only check the content of actual files when they exist in the expected directory
actualDir must haveSameFilesContentAs(expectedDir)

}}


##### Lines contents

Files are not the only possible source of lines and it is useful to be able to check the content of a `File` with a `Seq[String]`: ${snippet{

file1 must haveSameLinesAs(Seq(line1, line2, line3))

}}

This is because those 2 types implement the `org.specs2.text.LinesContent` trait, defining:

 * a name for the overall content
 * a method for returning the lines
 * a default method for computing the differences of 2 sequences of lines (in case you need to override this logic)

So if you have a specific type `T` which you can represent as a `Seq[String]`, you can create an implicit `LinesContent` and then you'll be able to use the `ContentMatchers`: ${snippet{

implicit def linesforMyType[T]: LinesContent[T] = new LinesContent[T] {
  def name(t: T) = "My list of lines"
  def lines(t: T): Seq[String] = Seq()// your implementation goes here
}

}}

"""
  lazy val (file1, file2) = (new File(""), new File(""))
  lazy val (actualDir, expectedDir) = (new File(""), new File(""))
  lazy val (line1, line2, line3) = ("", "", "")
}
