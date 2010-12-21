package org.specs2
package xml
import Nodex._
import scala.xml._
import scala.xml.NodeSeq._

class NodexSpec extends SpecificationWithJUnit { def is =

  "An isSpaceNode function should verify"                                                 ^
    { ! <a/>.isSpaceNode }                                                                ^
    { <a> </a>.child.last.isSpaceNode }                                                   ^
    "<a>\n</a>.child.last.isSpaceNode" ! { 
      <a>
      </a>.child.last.isSpaceNode
    }                                                                                     ^
    { Group(<a/><b/>).isSpaceNode must throwAn[UnsupportedOperationException].not }       ^
                                                                                          p^
  "An isEqualIgnoringSpace function should verify"                                        ^
    { <a/> ==/ <a/> }                                                                     ^
    { <a class="1"/> ==/ <a class="1"/> }                                                 ^
    { <a class="1" other="2"/> ==/ <a other="2" class="1"/> }                             ^
    { <a></a> ==/ <a></a> }                                                               ^
    { <a> </a> ==/ <a></a> }                                                              ^
    { <a>{"a"}{"b"}</a> ==/ <a>ab</a> }                                                   ^
    { <a>{"a"}{"b"}{"c"}</a> ==/ <a>abc</a> }                                             ^
    { <a>{"a"}{"b"}{"c"}{"d"}</a> ==/ <a>abcd</a> }                                       ^
    "<a><b/><c> </c></a> ==/ <a><b/><c></c></a>" ! {
      fromSeq(<a><b/><c> </c></a>.child.toList) ==/ 
      fromSeq(<a><b/><c></c></a>.child.toList) }                                          ^
    "! <a><b/><c>1</c></a> ==/ <a><b/><c></c></a>" ! {
      !(fromSeq(<a><b/><c>1</c></a>.child.toList) ==/ 
        fromSeq(<a><b/><c></c></a>.child.toList))
    }                                                                                     ^
    "<a>\n</a> ==/ <a></a>" ! {
      <a>
      </a> ==/ <a></a>
    }                                                                                     ^
    { <a><b/><c/></a> ==/ <a><c/><b/></a> }                                               ^
    { ! (<a>1</a> ==/ <a></a>) }                                                            ^
    { Text("1").isEqualIgnoringSpace(Text(" 1 ")) }                                       ^
    { !Text("1").isEqualIgnoringSpace(Text("2")) }                                        ^
                                                                                          p^
  "An isEqualIgnoringSpaceOrdered function should return"                                 ^
    { <a><b/><c/></a>.isEqualIgnoringSpaceOrdered(<a><b/><c/></a>) }                      ^
    { ! <a><b/><c/></a>.isEqualIgnoringSpaceOrdered(<a><c/><b/></a>) }
}    