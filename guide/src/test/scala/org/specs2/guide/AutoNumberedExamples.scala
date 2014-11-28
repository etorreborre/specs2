package org.specs2
package guide

import specification._

object AutoNumberedExamples extends UserGuidePage { def is = "Auto-numbered examples".title ^ s2"""

In the ${"named examples" ~/ NamedExamples} section we can see that there is a way to create groups of examples with standard names and use these names in the specification text (via the `s2` interpolated string). Here we go a step further.

The specification text is going to be some pure text, the examples will be described as example groups and the numbers will be created automatically. What does it look like? ${snippet{
class BulletedExamplesSpec extends script.Specification with Groups { def is = """
  This is an introduction

  First section
  =============

  A bit more text there.
   + and the first example
   + and the second example

  Second section
  =============

  Then another section
   + and the third example
   + and the fourth example

"""

  "first section" - new group {
    eg := 1 must_== 1
    eg := 1 must_== 1
  }

  "second section" - new group {
    eg := 1 must_== 1
  }
}
}}

This style of specification uses the `org.specs2.specification.script.Specification` class and mixes-in the `Groups` trait to create example groups. You can notice that:

 - Nothing is interpolated in the text that defines the `is` method
 - groups are anonymous, just called `group`
 - examples are anonymous, just called `eg` ([_exampli gratia_](http://en.wikipedia.org/wiki/List_of_Latin_phrases_(E)#exempli_gratia) in latin, meaning "for example")

At runtime, the text gets parsed and:

 - each Markdown header (underlined with `=====` or starting with `#`, `##`, `###`,... as per the markdown convention) marks the creation of a new example group
 - each text starting with `+` marks the creation of an example
 - groups and examples in the text are coupled with their counterpart in the code
 - if groups or examples are missing in the code, they are still created in the final specification but marked as `pending`
 - each group is enclosed in a `section` with the group name (automatically numbered, starting from `g1`)
 - each example is tagged with its group name, and its number (automatically numbered, starting from `e1`)

So, in the specification above the fourth example will be marked as `pending (g2.e2)` where `g2` is the group name and `e2` is the example name. As you can guess, sections and tags make it easy to just re-run parts of the specification without having to create those sections and tags yourself.

The major issue with this style of specification is that you cannot use your IDE to navigate from a piece of text to the corresponding code. You have to count the groups and count the number of examples to find the right one.

"""
}
