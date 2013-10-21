package org.specs2
package specification

import Fragments._

class FragmentsFormattingSpec extends script.Specification with Groups { def is = s2"""

 It is possible to use some tags to specify the formatting of text and example fragments

 When a section is tagged with a formatting tag, it must apply to all FormattedStrings of text and examples

 + for instance, a formatting section with verbatim = false and some text fragments
 + or a formatting section with markdown = false and some examples

"""

  "formatting" - new group {
    eg := formatFragments(formatSection(verbatim = false) ^ "t1" ^ "t2").fragments.collect(isSomeText).map(_.text.formatting.verbatim) must
           contain(allOf(false, false))

    eg := {
      val formattings = formatFragments(formatSection(markdown = false) ^
        "e1" ! ok ^
        "e2" ! ok ^
        formatSection(markdown = false) ^
        "e3" ! ok).fragments.collect(isAnExample).map(_.desc.formatting)

      formattings === Seq(Formatting(markdown = false), Formatting(markdown = false), Formatting())
    }
  }
}
