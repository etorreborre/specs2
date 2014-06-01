package org.specs2
package xml

import Nodex._
import scala.xml._
import scala.xml.NodeSeq._

class NodexSpec extends Specification { def is = s2"""

  isSpaceNode function returns true if a node only contains spaces
    ${ ! <a/>.isSpaceNode }
    ${ <a> </a>.child.last.isSpaceNode }
    <a>\n</a>.child.last is a space node
    >{
      <a>
      </a>.child.last.isSpaceNode
    }
    ${ Group(<a/><b/>).isSpaceNode must not(throwAn[UnsupportedOperationException]) }

  isEqualIgnoringSpace returns true if 2 NodeSeq are without spaces nodes are equal
    ${ <a/> ==/ <a/> }
    ${ <a class="1"/> ==/ <a class="1"/> }
    ${ <a class="1" other="2"/> ==/ <a other="2" class="1"/> }
    ${ <a></a> ==/ <a></a> }
    ${ <a> </a> ==/ <a></a> }
    ${ <a>{"a"}{"b"}</a> ==/ <a>ab</a> }
    ${ <a>{"a"}{"b"}{"c"}</a> ==/ <a>abc</a> }
    ${ <a>{"a"}{"b"}{"c"}{"d"}</a> ==/ <a>abcd</a> }
    ${ <a><b/><c> </c></a> ==/ <a><b/><c></c></a> }
    ${ fromSeq(<a><b/><c> </c></a>.child.toList) ==/ fromSeq(<a><b/><c></c></a>.child.toList) }
    ! <a><b/><c>1</c></a> ==/ <a><b/><c></c></a>
    ${ !(fromSeq(<a><b/><c>1</c></a>.child.toList) ==/ fromSeq(<a><b/><c></c></a>.child.toList)) }
    "<a>\n</a> ==/ <a></a>"
    ${
      <a>
      </a> ==/ <a></a>
    }
    ${ <a><b/><c/></a> ==/ <a><c/><b/></a> }
    ${ ! (<a>1</a> ==/ <a></a>) }
    ${ scala.xml.Text("1") isEqualIgnoringSpace scala.xml.Text(" 1 ") }
    ${ !(scala.xml.Text("1") isEqualIgnoringSpace scala.xml.Text("2")) }

  isEqualIgnoringSpaceOrdered returns true if 2 NodeSeqs are the same regardless of spaces and order
    ${ <a><b/><c/></a> isEqualIgnoringSpaceOrdered <a><b/><c/></a> }
    ${ !(<a><b/><c/></a> isEqualIgnoringSpaceOrdered <a><c/><b/></a>) }
                                                                                                            """
}