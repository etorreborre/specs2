package org.specs2
package matcher

import scala.xml._
import specification.Groups

class XmlMatchersSpec extends Specification with Groups { def is = s2"""

 A equals ignore spaces matcher should
   match nodes, even if there are spaces
   ${ <a><b/></a> must ==/(<a> <b/></a>) }
   have an beEqualToIgnoringSpace as an alias
   ${ <a><b/></a> must beEqualToIgnoringSpace(<a> <b/></a>) }

   match if the nodes are not in the same order
   ${ <a><b/><c/></a> must ==/(<a> <c/><b/></a>) }
   ${ (<a><b/><c/></a>:NodeSeq) must ==/(<a> <c/><b/></a>) }
   ${ <a><b/><c/></a> must be_==/(<a> <c/><b/></a>) }
   ${ <a><b/><c/></a> must be ==/(<a> <c/><b/></a>) }

   match if the attributes are not in the same order
   ${ <a><b t="1" s="2"/><c/></a> must ==/(<a> <c/><b s="2" t="1"/></a>) }
   not match when there is evaluation of != elements
   ${ <a>{"a"}</a> must not ==/(<a>{"b"}</a>) }
   provide a way to specify that the comparison should be ordered
   ${ <a><c/> <b/></a> must ==/(<a> <c/><b/></a>).ordered }

   match if there are newlines                                                                              ${g1().e1}
   match if attributes are not in the right order                                                           ${g1().e2}
   fail if 2 nodes are not equal, even ignoring spaces                                                      ${g1().e3}
   fail if 2 nodes are a Text and an Atom with different data                                               ${g1().e4}

   provide be + matchers forms
   ${ <a><b/></a> must be equalToIgnoringSpace(<a> <b/></a>) }
   ${ <a><b/></a> must be ==/(<a> <b/></a>) }
   ${ <n a="1" b="2"/> must be ==/(<n b="2" a="1"/>) }
   ${ <a><b/></a> must not be ==/(<b></b>) }

 A \\ matcher should match if a node is a direct child of another
   ${ <a><b/></a> must \("b") }
   ${ <a><b></b></a> must \("b") }
   ${ <a>hello</a> must \\("a").textIs("hello") }
   ${ <a>hello</a> must \\("a") \> ("hello") }
   ${ (<a>hello</a> must \\("a") \> ("world")) returns "<a>hello</a> doesn't contain node 'a' with text: world"}
   ${ <a>hello</a> must \\("a").textMatches("h.*") }
   ${ <a>hello</a> must \\("a") \>~ ("h.*") }
   ${ <a><b><c></c></b></a> must \(<b><c></c></b>) }
   checking attribute names
   ${ <a><b name="value"></b></a> must \("b", "name") }
   ${ <a><b name="value" name2="value"></b></a> must \("b", "name2", "name") }
   checking attribute values
   ${ <a><b name="value"></b></a> must \("b", "name"->"value") }
   ${ <a><b n="v" n2="v2" n3="v3"></b></a> must \("b", "n"->"v", "n2"->"v2") }

 A \\ matcher should not match a node
   when the source has no children                                                                          ${g2().e1}
   when the searched node is not a direct child                                                             ${g2().e2}
   when an attribute name is missing                                                                        ${g2().e3}
   when the attribute is ok, but the value is ko                                                            ${g2().e4}
   when the attribute is ko, but the value is ok                                                            ${g2().e5}
   when the attribute is ko, and the value is ko                                                            ${g2().e6}
   when matching exactly and an attribute is missing                                                        ${g2().e7}
   when the searched node contains unmatching nodes                                                         ${g2().e8}

 A \\\\ matcher should match if a node is a nested child of another
   ${ <a></a> must \\("a") }
   ${ <a><b/></a> must \\("b") }
   ${ <a><b></b></a> must \\("b") }
   ${ <a><b><c></c></b></a> must \\(<b><c></c></b>) }
   ${ <a><s><c></c></s></a> must \\("c") }
   checking attribute names
   ${ <a><b name="value"></b></a> must \\("b", "name") }
   ${ <a><b name="value" name2="value"></b></a> must \\("b", "name2", "name") }
   checking attribute values
   ${ <a><b name="value"></b></a> must \\("b", "name"->"value") }
   ${ <a><b n="v" n2="v2" n3="v3"></b></a> must \\("b", "n"->"v", "n2"->"v2") }

 A \\\\ matcher should not match a node
   when an attribute name is missing                                                                        ${g3().e1}
   when the attribute is ok, but the value is ko                                                            ${g3().e2}
   when the attribute is ko, but the value is ok                                                            ${g3().e3}
   when the attribute is ko, and the value is ko                                                            ${g3().e4}
   when matching exactly and an attribute is missing                                                        ${g3().e5}
   when the searched node contains unmatching nodes                                                         ${g3().e6}
   when it doesn't contain the same text                                                                    ${g3().e7}
   when it doen't contain the same text even when one is an Atom and the other a Text                       ${g3().e8}
                                                                                                                        """

  "eis" - new g1 {
     e1 := <a> <b/></a> must ==/ {<a>
               <b/>

             </a>}

    e2 := <a><b n1="n1" n2="n2"/></a> must ==/(<a><b n2="n2" n1="n1"/></a>)

    e3 := (<a><b/></a> must ==/(<a> <c/></a>)) returns
          "<a><b/></a> is not equal to <a> <c/></a>"
    e4 := (new Atom("hello").toSeq aka "the seq" must ==/(new Text("world").toSeq)) returns
          "the seq 'hello' is not equal to world"

  }

  "firstChild" - new g2 {
    e1 := (<a></a> must \("a")) returns "<a></a> doesn't contain subnode 'a'"
    e2 := (<a><b><c></c></b></a> must \("c")) returns
                "<a><b><c></c></b></a> doesn't contain subnode 'c'"
    e3 := (<a><b name2="value"></b></a> must \("b", "name")) returns
          "<a><b name2=\"value\"></b></a> doesn't contain subnode 'b' with attributes: name"
    e4 := (<a><b n="v"></b></a> must \("b", "n"->"v1")) returns
          "<a><b n=\"v\"></b></a> doesn't contain subnode 'b' with attributes: n=\"v1\""
    e5 :=	(<a><b n="v"></b></a> must \("b", "n1"->"v")) returns
          "<a><b n=\"v\"></b></a> doesn't contain subnode 'b' with attributes: n1=\"v\""
    e6 := (<a><b n="v"></b></a> must \("b", "n"->"v", "n2"->"v2")) returns
          "<a><b n=\"v\"></b></a> doesn't contain subnode 'b' with attributes: n=\"v\" n2=\"v2\""
    e7 := (<a><b n="v" n2="v"></b></a> must \("b", "n"->"v").exactly) returns
          "<a><b n=\"v\" n2=\"v\"></b></a> doesn't contain subnode 'b' with exactly the attributes: n=\"v\""
    e8 := (<a><b><c></c></b></a> must \(<b><d></d></b>)) returns
                "<a><b><c></c></b></a> doesn't contain <b><d></d></b>"
  }

  "deepChild" - new g3 {
    e1 := (<a><b name2="value"></b></a> must \\("b", "name")) returns
          "<a><b name2=\"value\"></b></a> doesn't contain node 'b' with attributes: name"
    e2 := (<a><b n="v"></b></a> must \\("b", "n"->"v1")) returns
          "<a><b n=\"v\"></b></a> doesn't contain node 'b' with attributes: n=\"v1\""
    e3 :=	(<a><b n="v"></b></a> must \\("b", "n1"->"v")) returns
          "<a><b n=\"v\"></b></a> doesn't contain node 'b' with attributes: n1=\"v\""
    e4 := (<a><b n="v"></b></a> must \\("b", "n"->"v", "n2"->"v2")) returns
          "<a><b n=\"v\"></b></a> doesn't contain node 'b' with attributes: n=\"v\" n2=\"v2\""
    e5 := (<a><b n="v" n2="v"></b></a> must \\("b", "n"->"v").exactly) returns
          "<a><b n=\"v\" n2=\"v\"></b></a> doesn't contain node 'b' with exactly the attributes: n=\"v\""
    e6 := (<a><b><c></c></b></a> must \\(<b><d></d></b>)) returns
          "<a><b><c></c></b></a> doesn't contain <b><d></d></b>"
    e7 := (<a><b>hello</b></a> must \\(<b>world</b>)) returns
	        "<a><b>hello</b></a> doesn't contain <b>world</b>"
    e8 := (<a><b>{"hello"}</b></a> must \\(<b>world</b>)) returns
	        "<a><b>hello</b></a> doesn't contain <b>world</b>"
  }
}
