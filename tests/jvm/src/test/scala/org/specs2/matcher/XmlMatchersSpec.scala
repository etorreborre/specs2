package org.specs2
package matcher

import scala.xml.*

class XmlMatchersSpec extends Spec with XmlMatchers with MustExpectations {
  def is = s2"""

 A equals ignore spaces matcher should
   match nodes, even if there are spaces
   ${<a><b/></a> must ==/(<a> <b/></a>)}
   have an beEqualToIgnoringSpace as an alias
   ${<a><b/></a> must beEqualToIgnoringSpace(<a> <b/></a>)}

   match if the nodes are not in the same order
   ${<a><b/><c/></a> must ==/(<a> <c/><b/></a>)}
   ${(<a><b/><c/></a>: NodeSeq) must ==/(<a> <c/><b/></a>)}
   ${<a><b/><c/></a> must be_==/(<a> <c/><b/></a>)}

   match if the attributes are not in the same order
   ${<a><b t="1" s="2"/><c/></a> must ==/(<a> <c/><b s="2" t="1"/></a>)}
   not match when there is evaluation of != elements
   ${<a>{"a"}</a> must not(==/(<a>{"b"}</a>))}
   provide a way to specify that the comparison should be ordered
   ${<a><c/> <b/></a> must ==/(<a> <c/><b/></a>).ordered}

   match if there are newlines $match1
   match if attributes are not in the right order $match2
   fail if 2 nodes are not equal, even ignoring spaces $match3
   fail if 2 nodes are a Text and an Atom with different data $match4

   provide be + matchers forms
   ${<a><b/></a> must beEqualToIgnoringSpace(<a> <b/></a>)}
   ${<a><b/></a> must be_==/(<a> <b/></a>)}
   ${<n a="1" b="2"/> must be_==/(<n b="2" a="1"/>)}
   ${<a><b/></a> must not(==/(<b></b>))}

 A \\ matcher should match if a node is a direct child of another
   ${<a><b/></a> must \("b")}
   ${<a><b></b></a> must \("b")}
   ${<a>hello</a> must \\("a").textIs("hello")}
   ${<a>hello</a> must \\("a") \> ("hello")}
   ${(<a>hello</a> must \\("a") \> ("world")) returns "<a>hello</a> doesn't contain node 'a' with text: world"}
   ${<a>hello</a> must \\("a").textMatches("h.*")}
   ${<a>hello</a> must \\("a") \>~ ("h.*")}
   ${<a><b><c></c></b></a> must \(<b><c></c></b>)}
   checking attribute names
   ${<a><b name="value"></b></a> must \("b", "name")}
   ${<a><b name="value" name2="value"></b></a> must \("b", "name2", "name")}
   checking attribute values
   ${<a><b name="value"></b></a> must \("b", "name" -> "value")}
   ${<a><b n="v" n2="v2" n3="v3"></b></a> must \("b", "n" -> "v", "n2" -> "v2")}

 A \\ matcher should not match a node
   when the source has no children $path1
   when the searched node is not a direct child $path2
   when an attribute name is missing $path3
   when the attribute is ok, but the value is ko $path4
   when the attribute is ko, but the value is ok $path5
   when the attribute is ko, and the value is ko $path6
   when matching exactly and an attribute is missing $path7
   when the searched node contains unmatching nodes $path8

 A \\\\ matcher should match if a node is a nested child of another
   ${<a></a> must \\("a")}
   ${<a><b/></a> must \\("b")}
   ${<a><b></b></a> must \\("b")}
   ${<a><b><c></c></b></a> must \\(<b><c></c></b>)}
   ${<a><s><c></c></s></a> must \\("c")}
   checking attribute names
   ${<a><b name="value"></b></a> must \\("b", "name")}
   ${<a><b name="value" name2="value"></b></a> must \\("b", "name2", "name")}
   checking attribute values
   ${<a><b name="value"></b></a> must \\("b", "name" -> "value")}
   ${<a><b n="v" n2="v2" n3="v3"></b></a> must \\("b", "n" -> "v", "n2" -> "v2")}

 A \\\\ matcher should not match a node
   when an attribute name is missing $deepPath1
   when the attribute is ok, but the value is ko $deepPath2
   when the attribute is ko, but the value is ok $deepPath3
   when the attribute is ko, and the value is ko $deepPath4
   when matching exactly and an attribute is missing $deepPath5
   when the searched node contains unmatching nodes $deepPath6
   when it doesn't contain the same text $deepPath7
   when it doesn't contain the same text even when one is an Atom and the other a Text $deepPath8
"""

  def match1 = <a> <b/></a> must ==/ {
    <a>
            <b/>
          </a>
  }
  def match2 = <a><b n1="n1" n2="n2"/></a> must ==/(<a><b n2="n2" n1="n1"/></a>)

  def match3 = (<a><b/></a> must ==/(<a> <c/></a>)) returns
    "<a><b/></a> is not equal to <a> <c/></a>"

  def match4 = (new Atom("hello").toSeq `aka` "the seq" must ==/(new Text("world").toSeq)) returns
    "the seq 'hello' is not equal to world"

  def path1 = (<a></a> must \("a")) returns "<a></a> doesn't contain subnode 'a'"

  def path2 = (<a><b><c></c></b></a> must \("c")) returns
    "<a><b><c></c></b></a> doesn't contain subnode 'c'"

  def path3 = (<a><b name2="value"></b></a> must \("b", "name")) returns
    "<a><b name2=\"value\"></b></a> doesn't contain subnode 'b' with attributes: name"

  def path4 = (<a><b n="v"></b></a> must \("b", "n" -> "v1")) returns
    "<a><b n=\"v\"></b></a> doesn't contain subnode 'b' with attributes: n=\"v1\""

  def path5 = (<a><b n="v"></b></a> must \("b", "n1" -> "v")) returns
    "<a><b n=\"v\"></b></a> doesn't contain subnode 'b' with attributes: n1=\"v\""

  def path6 = (<a><b n="v"></b></a> must \("b", "n" -> "v", "n2" -> "v2")) returns
    "<a><b n=\"v\"></b></a> doesn't contain subnode 'b' with attributes: n=\"v\" n2=\"v2\""

  def path7 = (<a><b n="v" n2="v"></b></a> must \("b", "n" -> "v").exactly) returns
    "<a><b n=\"v\" n2=\"v\"></b></a> doesn't contain subnode 'b' with exactly the attributes: n=\"v\""

  def path8 = (<a><b><c></c></b></a> must \(<b><d></d></b>)) returns
    "<a><b><c></c></b></a> doesn't contain <b><d></d></b>"

  def deepPath1 = (<a><b name2="value"></b></a> must \\("b", "name")) returns
    "<a><b name2=\"value\"></b></a> doesn't contain node 'b' with attributes: name"

  def deepPath2 = (<a><b n="v"></b></a> must \\("b", "n" -> "v1")) returns
    "<a><b n=\"v\"></b></a> doesn't contain node 'b' with attributes: n=\"v1\""

  def deepPath3 = (<a><b n="v"></b></a> must \\("b", "n1" -> "v")) returns
    "<a><b n=\"v\"></b></a> doesn't contain node 'b' with attributes: n1=\"v\""

  def deepPath4 = (<a><b n="v"></b></a> must \\("b", "n" -> "v", "n2" -> "v2")) returns
    "<a><b n=\"v\"></b></a> doesn't contain node 'b' with attributes: n=\"v\" n2=\"v2\""

  def deepPath5 = (<a><b n="v" n2="v"></b></a> must \\("b", "n" -> "v").exactly) returns
    "<a><b n=\"v\" n2=\"v\"></b></a> doesn't contain node 'b' with exactly the attributes: n=\"v\""

  def deepPath6 = (<a><b><c></c></b></a> must \\(<b><d></d></b>)) returns
    "<a><b><c></c></b></a> doesn't contain <b><d></d></b>"

  def deepPath7 = (<a><b>hello</b></a> must \\(<b>world</b>)) returns
    "<a><b>hello</b></a> doesn't contain <b>world</b>"

  def deepPath8 = (<a><b>{"hello"}</b></a> must \\(<b>world</b>)) returns
    "<a><b>hello</b></a> doesn't contain <b>world</b>"
}
