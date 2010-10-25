package org.specs2
package xml
import Nodex._
import scala.xml._
import scala.xml.NodeSeq._

class NodexSpec extends SpecificationWithJUnit { def is =
  "An isSpaceNode function should return" ^
    "false for a node with a simple label" ! {
      <a/>.isSpaceNode must beFalse
    }^
    "true for a node containing a space" ! {
      <a> </a>.child.last.isSpaceNode must beTrue
    }^
    "true for a node containing a newline and spaces" ! {
      <a>
        </a>.child.last.isSpaceNode must beTrue
    }^
    "not fail with a Group" ! {
      Group(<a/><b/>).isSpaceNode must throwAn[UnsupportedOperationException].not
    }^
  p^
  "An isEqualIgnoringSpace function should return"^
    "true for <a> ==/ <a>" ! {
      <a/> ==/ <a/> must beTrue
    }^
    """true for <a class="1"> ==/ <a class="1">""" ! {
      <a class="1"/> ==/ <a class="1"/> must beTrue
    }^
    """true for <a class="1" other="2"> ==/ <a other="2" class="1">""" ! {
      <a class="1" other="2"/> ==/ <a other="2" class="1"/> must beTrue
    }^
    "true for <a></a> ==/ <a></a>" ! {
      <a></a> ==/ <a></a> must beTrue
    }^
    "true for <a> </a> ==/ <a></a>" ! {
      <a> </a> ==/ <a></a> must beTrue
    }^
    "true for <a>{a}{b}</a> ==/ <a>ab</a>" ! {
      <a>{"a"}{"b"}</a> ==/ <a>ab</a> must beTrue
    }^
    "true for <b/><c> </c> ==/ <b/><c></c>" ! {
      fromSeq(<a><b/><c> </c></a>.child.toList) ==/ fromSeq(<a><b/><c></c></a>.child.toList) must beTrue
    }^
    "false for <b/><c>1</c> ==/ <b/><c></c>" ! {
      fromSeq(<a><b/><c>1</c></a>.child.toList) ==/ fromSeq(<a><b/><c></c></a>.child.toList) must beFalse
    }^
    "true for <a>\n</a> ==/ <a></a>" ! {
      <a>
      </a> ==/ <a></a> must beTrue
    }^
    "true for unordered sequences of nodes <a><b/><c/></a> ==/ <a><c/><b/></a>" ! {
      <a><b/><c/></a> ==/ <a><c/><b/></a> must beTrue
    }^
    "false for <a>1</a> ==/ <a></a>" ! {
      <a>1</a> ==/ <a></a> must beFalse
    }^
    "true for Text(1) ==/ Text( 1 )" ! {
      Text("1").isEqualIgnoringSpace(Text(" 1 ")) must beTrue
    }^
    "false for Text(1) ==/ Text(2)" ! {
      Text("1").isEqualIgnoringSpace(Text("2")) must beFalse
    }^
  p^
  "An isEqualIgnoringSpaceOrdered function should return" ^
    "true for <a><b/><c/></a> and <a><b/><c/></a>" ! {
      <a><b/><c/></a>.isEqualIgnoringSpaceOrdered(<a><b/><c/></a>) must beTrue
    }^
    "false for <a><b/><c/></a> and <a><c/><b/></a>" ! {
      <a><b/><c/></a>.isEqualIgnoringSpaceOrdered(<a><c/><b/></a>) must beFalse
    }
}