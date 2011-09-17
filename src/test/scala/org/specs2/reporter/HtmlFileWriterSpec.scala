package org.specs2
package reporter
import io._
import mock._
import specification._
import matcher.DataTables

class HtmlFileWriterSpec extends Specification with Mockito { outer => def is =
                                                                                                                        """
The HtmlFileWriter class is responsible for writing a html reports to disk.
                                                                                                                        """^p^
  "Resources"                                                                                                           ^
    "there must be a directory for css files"                                                                           ! resources().css^
    "there must be a directory for images files"                                                                        ! resources().images^
    "there must be a directory for the js tree theme files"                                                             ! resources().jstheme^
                                                                                                                        end
                                                                                          
  implicit val argument = args()

  case class resources() extends MockHtmlFileWriter {
    writer.writeFiles(Seq())
    
    def css = there was one(fs).copySpecResourcesDir(===("css"), anyString)
    def images = there was one(fs).copySpecResourcesDir(===("images"), anyString)
    def jstheme = there was one(fs).copySpecResourcesDir(===("css/themes/default"), anyString)
  }

  trait MockHtmlFileWriter extends FragmentExecution with DefaultStoring { outer =>
    val fs = mock[FileSystem]
    val fileWriter = new MockFileWriter {}
    val out = fileWriter.getWriter
    def writer = new HtmlFileWriter { 
			override lazy val fileSystem = fs
			override lazy val fileWriter = outer.fileWriter
		}

  }

}