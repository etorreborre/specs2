package org.specs2
package specification

import reporter.DefaultSelection
import matcher.ThrownExpectations

class TagsSpec extends SpecificationWithJUnit with ThrownExpectations { def is =
                                                                                                                        """
A specification can be tagged with some meaningful names like "integration" or "accounts". Creating tags amounts to
adding new fragments in the specification. Then those fragments are used to determine which other fragments must be executed
during the specification execution. There are 2 types of tags:

 * `TaggedAs(name1, name2,...)` marks the previous fragment as tagged with the given names
 * `Tag(name1, name2,...)` marks the next fragment as tagged with the given names
 * `AsSection(name1, name2,...)` marks the previous fragment and the next as tagged with the given names,
    until the next `Section(name1, name2)` tag
 * `Section(name1, name2,...)` marks the next fragments as tagged with the given names, until the next `Section(name1, name2)` tag

                                                                                                                        """^
                                                                                                                        p^
  "A TaggedAs(t1) fragment can be created using the tag method in an Acceptance specification"                          ^
    "then, when using exclude='t1'"                                                                                     ^
      "the tagged fragment is excluded from the selection"                                                              ! tag1^
       "and other fragments are kept"                                                                                   ! tag2^
    "then, when using include='t1'"                                                                                     ^
      "the tagged fragment is included in the selection"                                                                ! tag3^
      "and other fragments are excluded"                                                                                ! tag4^
                                                                                                                        endp^
  "A AsSection(t1) fragment can be created using the section method in an Acceptance specification"                     ^
    "then, when using exclude='t1'"                                                                                     ^
      "the tagged fragments just before and after the section tag are excluded from the selection"                      ! section1^
      "and the fragments before the section are kept"                                                                   ! section2^
      "if the section is closed with another AsSection fragment containing the tag t1"                                  ^
        "the tagged fragments between the section tags are excluded"                                                    ! section3^
        "and the fragments outside the section are kept"                                                                ! section4^
    "then, when using include='t1'"                                                                                     ^
      "the tagged fragments just before and after the section tag are included in the selection"                        ! section5^
      "and the fragments before the section are excluded"                                                               ! section6^
      "if the section is closed with another AsSection fragment containing the tag t1"                                  ^
        "the tagged fragments between the section tags are included"                                                    ! section7^
        "and the fragments outside the section are excluded"                                                            ! section8^
                                                                                                                        end

  import DefaultSelection._
  import Tags._

  val tagged =
    "text" ^
      "e1" ! success ^ tag("t1")^
      "e2" ! success ^ end

  val sectioned =
    "text" ^
      "e1" ! success ^
      "e2" ! success ^ section("t1")^
      "e3" ! success ^
      "e4" ! success ^ section("t1")^
      "e5" ! success ^ end

  def includeTag(fs: Fragments) = select(args(include="t1"))(fs).map(_.toString)
  def excludeTag(fs: Fragments) = select(args(exclude="t1"))(fs).map(_.toString)

  def tag1 = excludeTag(tagged) must not containMatch("e1")
  def tag2 = excludeTag(tagged) must containMatch("e2")
  def tag3 = includeTag(tagged) must not containMatch("e2")
  def tag4 = includeTag(tagged) must containMatch("e1")

  def section1 = excludeTag(sectioned) must containMatch("e1")
  def section2 = excludeTag(sectioned) must not containMatch("e2")
  def section3 = excludeTag(sectioned) must not containMatch("e3")
  def section4 = excludeTag(sectioned) must containMatch("e5")

  def section5 = includeTag(sectioned) must not containMatch("e1")
  def section6 = includeTag(sectioned) must containMatch("e2")
  def section7 = includeTag(sectioned) must containMatch("e3")
  def section8 = includeTag(sectioned) must not containMatch("e5")
}
