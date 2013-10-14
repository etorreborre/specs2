package org.specs2
package specification

import main._
import reporter.DefaultSelection
import matcher.ThrownExpectations

class TagsSpec extends Specification with ThrownExpectations with Tags { def is = s2"""

A specification can be tagged with some meaningful names like "integration" or "accounts". Creating tags amounts to
adding new fragments in the specification. Then those fragments are used to determine which other fragments must be executed
during the specification execution. There are 2 types of tags for marking a single fragment or a full range of fragments:

 * `TaggedAs(name1, name2,...)` marks the previous fragment as tagged with the given names
 * `Tag(name1, name2,...)` marks the next fragment as tagged with the given names

 * `AsSection(name1, name2,...)` marks the previous fragment and the next as tagged with the given names,
    until the next `Section(name1, name2)` tag
 * `Section(name1, name2,...)` marks the next fragments as tagged with the given names, until the next `Section(name1, name2)` tag

                                                                                                                        
                                                                                                                        
 A TaggedAs(t1) fragment can be created using the tag method in an Acceptance specification
   then, when using exclude='t1'
     the tagged fragment is excluded from the selection                                             ${tag1}
      and other fragments are kept                                                                  ${tag2}
   then, when using include='t1'
     the tagged fragment is included in the selection                                               ${tag3}
     and other fragments are excluded                                                               ${tag4}
     a fragment with several names is also included                                                 ${tag5}
     a SpecStart is not excluded                                                                    ${tag6}
     a SpecEnd is not excluded                                                                      ${tag7}

 A AsSection(t1) fragment can be created using the section method in an Acceptance specification
   then, when using exclude='t1'
     the tagged fragments just before and after the section tag are excluded from the selection     ${section1}
     and the fragments before the section are kept                                                  ${section2}
     if the section is closed with another AsSection fragment containing the tag t1
       the tagged fragments between the section tags are excluded                                   ${section3}
       and the fragments outside the section are kept                                               ${section4}
   then, when using include='t1'
     the tagged fragments just before and after the section tag are included in the selection       ${section5}
     and the fragments before the section are excluded                                              ${section6}
     if the section is closed with another AsSection fragment containing the tag t1
       the tagged fragments between the section tags are included                                   ${section7}
       and the fragments outside the section are excluded                                           ${section8}
   then, when using several tags in the section
     opening and closing a section with the same tags                                               ${section9}
     opening and closing a section with different tags                                              ${section10}

 Tags can also be used in a mutable specification
   a tag call on the line before an example will mark it                                            ${mutabletags().e1}
   a tag call on the same line as an example will mark it                                           ${mutabletags().e2}
   a section call on the same line as a should block will mark all the examples                     ${mutabletags().e3}

 Tags can be specified from arguments
   from system properties                                                                           ${fromargs().e1}
                                                                                                    """

  import DefaultSelection._
  import Tags._

  val tagged =
    xonly  ^
    "text" ^
      "e1" ! success ^ tag("t1")^
      "e2" ! success ^ end

  val tagged2 =
    "text" ^
      "e1" ! success ^ tag("t1", "t2") ^
      "e2" ! success ^ end

  val sectioned =
    "text" ^
      "e1" ! success ^
      "e2" ! success ^ section("t1")^
      "e3" ! success ^
      "e4" ! success ^ section("t1")^
      "e5" ! success ^ end

  val sectionedMulti =
    "text" ^
      "e1" ! success ^
      "e2" ! success ^ section("t1", "t2")^
      "e3" ! success ^
      "e4" ! success ^ section("t1", "t2")^
      "e5" ! success ^ end

  val sectionedMulti2 =
    "text" ^
      "e1" ! success ^
      "e2" ! success ^ section("t1", "t2", "t3")^
      "e3" ! success ^
      "e4" ! success ^ section("t1", "t2")^
      "e5" ! success ^ end

  def includeTag(fs: Fragments) = includeTags(fs, "t1")
  def excludeTag(fs: Fragments) = excludeTags(fs, "t1")
  def includeTags(fs: Fragments, tags: String*) = withTags(fs, args(include=tags.mkString(",")))
  def excludeTags(fs: Fragments, tags: String*) = withTags(fs, args(exclude=tags.mkString(",")))
  def withTags(fs: Fragments, args: Arguments) = select(args)(SpecificationStructure(fs)).content.fragments.map(_.toString)

  def includeMatch(fs: Fragments, tags: String, names: String*) = {
    names must contain((n:String) => includeTags(fs, tags.split(","):_*) must contain(=~(n))).forall
  }
  def excludeMatch(fs: Fragments, tags: String, names: String*) = {
    names must contain((n:String) => excludeTags(fs, tags.split(","):_*) must contain(=~(n))).forall
  }
  def includeDoesntMatch(fs: Fragments, tags: String, names: String*) = {
    names must not contain((n:String) => includeTags(fs, tags.split(","):_*) must contain(=~(n)))
  }
  def excludeDoesntMatch(fs: Fragments, tags: String, names: String*) = {
    names must not contain((n:String) => excludeTags(fs, tags.split(","):_*) must contain(=~(n)))
  }
  def includeMustSelect(fs: Fragments, tags: String, included: String, excluded: String) = {
    includeMatch(fs, tags, included) and includeDoesntMatch(fs, tags, excluded)
  }
  def excludeMustSelect(fs: Fragments, tags: String, included: String, excluded: String) = {
    excludeMatch(fs, tags, included) and excludeDoesntMatch(fs, tags, excluded)
  }

  def tag1 = excludeDoesntMatch(tagged, "t1", "e1")
  def tag2 = excludeMatch(tagged, "t1", "e2")
  def tag3 = includeDoesntMatch(tagged , "t1", "e2")
  def tag4 = includeMatch(tagged , "t1", "e1")
  def tag5 = includeMatch(tagged2, "t1", "e1")
  def tag6 = includeMatch(tagged , "t1", "SpecStart")
  def tag7 = includeMatch(tagged , "t1", "SpecStart")

  def section1 = excludeMatch(sectioned, "t1", "e1")
  def section2 = excludeDoesntMatch(sectioned, "t1", "e2")
  def section3 = excludeDoesntMatch(sectioned, "t1", "e3")
  def section4 = excludeMatch(sectioned, "t1", "e5")

  def section5 = includeDoesntMatch(sectioned, "t1", "e1")
  def section6 = includeMatch(sectioned, "t1", "e2")
  def section7 = includeMatch(sectioned, "t1", "e3")
  def section8 = includeDoesntMatch(sectioned, "t1", "e5")
  def section9 = includeMatch(sectionedMulti, "t1", "e2", "e3", "e4") and
                 includeDoesntMatch(sectionedMulti, "t1", "e1", "e5")

  def section10 = includeMatch(sectionedMulti2, "t2", "e2", "e3", "e4") and
                  includeDoesntMatch(sectionedMulti2, "t1", "e1") and
                  includeMatch(sectionedMulti2, "t3", "e5")

  case class mutabletags() {
    val tagged = new org.specs2.mutable.Specification with org.specs2.mutable.Tags {
      "text" should {
        tag("t1")
        "e1" in success
        "e2" in success tag("t2")
        "e3" in success
      }
      "other text" should {
        "e4" in success
        "e5" in success
      } section("t3")

    }
    def e1 = includeMustSelect(tagged.content, tags="t1", included="e1", excluded="e2")
    def e2 = includeMustSelect(tagged.content, tags="t2", included="e2", excluded="e1")
    def e3 = includeMustSelect(tagged.content, tags="t3", included="e4", excluded="e3")
  }

  case class fromargs() {
    def e1 = {
      val properties = new MapSystemProperties {
        lazy val properties = Map("specs2.include" -> "t1", "specs2.exclude" -> "")
      }
      val arguments = Arguments.extract(Seq(), properties)
      select(arguments)(SpecificationStructure(tagged)).fragments.fragments.map(_.toString) must containMatch("e1")
    }
  }
}
