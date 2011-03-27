package org.specs2
package specification

import reporter.DefaultSelection

class TagsSpec extends SpecificationWithJUnit { def is =
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
  "A TaggedAs fragment can be created using the tag method in an Acceptance specification"                              ^
    "then the fragment can be excluded from the selection"                                                              ! e1^
                                                                                                                        end

  import DefaultSelection._
  def e1 = {
    import Tags._
    val fragments =
      "text" ^
        "e1" ! success ^ tag("t1")^
        "e2" ! success ^ end
    select(args(exclude="t1"))(fragments).map(_.toString) must not containMatch("e1")
  }
}
