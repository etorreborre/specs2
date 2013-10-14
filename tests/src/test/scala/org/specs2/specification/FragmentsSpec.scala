package org.specs2
package specification

import Fragments._
import matcher.ThrownExpectations
import org.specs2.specification.TagsFragments.AsSection

class FragmentsSpec extends org.specs2.mutable.script.Specification with Groups with ThrownExpectations { s2"""

 A Fragments object contains all the fragments for a given specification.
 It usually has a SpecStart and a SpecEnd and a middle section

 it is possible to append other fragments to the middle section
 + if a piece text is added, it must be concatenated with the previous piece of text if any
 + if fragments with inside links are added, the links must be preserved
 + if linked fragments are added, the texts must not be fused

 + a Fragments object with several tags can be "compacted" so that consecutive sections are collapsed into one
 """

 "append" - new group {
   eg := helloFragments.append(createList(Text(" world"))).texts.headOption must beSome(Text("hello world"))

   eg := {
     val fs = helloFragments.append(create(SpecStart(specName)).linkIs(UrlHtmlLink("a link")).add(Text(" world")).add(SpecEnd(specName)))
     fs.starts.headOption.flatMap(_.link.map(_.url)) must beSome("a link")
   }

   eg := {
     val fs = helloFragments.append(create(SpecStart(specName)).linkIs(UrlHtmlLink("a link")).add(Text(" world")).add(SpecEnd(specName)))
     fs.texts must contain(=~("hello") ^^ fragmentToString)
     fs.texts must not(contain(=~("hello world") ^^ fragmentToString))
   }

   eg := {
     val fs = (helloFragments ^ section("t1", "t2") ^ section("t3")).compactTags
     fs.fragments.collect { case s: AsSection => s.names }.flatten must contain(exactly("t1", "t2", "t3"))
   }
 }

  val fragmentToString = ((_:Fragment).toString)
  val helloFragments = createList(Text("hello"))
  val specName = SpecName("name")
}
