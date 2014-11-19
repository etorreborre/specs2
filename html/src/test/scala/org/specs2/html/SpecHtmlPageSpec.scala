package org.specs2
package html

import io._
import specification.core.SpecStructure

class SpecHtmlPageSpec extends Spec { def is = s2"""

 A toc that is added to a Spec html page must replace the <toc/> tag $toc
 When the toc is added the headers must be surrounded by anchors     $anchors

"""

  def toc = {
    val specPage =
      page(
        """
          |<html>
          |<head></head>
          |<body><toc/><h1>Title</h1></body>
          |</html>
        """.stripMargin).addToc(<ul><li>section 1</li></ul>)

    specPage.content must contain("li")
  }


  def anchors = {
    val specPage =
      page(
        """
          |<html>
          |<head></head>
          |<body><toc/><h1>Title</h1></body>
          |</html>
        """.stripMargin).addToc(<ul><li>section 1</li></ul>)

    specPage.content must contain("""<a name="Title"><h1>Title""")
  }

  def page(content: String): SpecHtmlPage =
    SpecHtmlPage(SpecStructure.empty(getClass), FilePath.unsafe("here"), DirectoryPath.unsafe("out"), content)
}
