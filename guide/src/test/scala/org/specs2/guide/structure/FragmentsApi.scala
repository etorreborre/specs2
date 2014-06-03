package org.specs2
package guide
package structure

class FragmentsApi extends UserGuidePage { def is = "Fragments API".title^ s2"""
### Presentation

The interpolated string shown in the [Quick Start](org.specs2.guide.QuickStart.html) is actually desugared to a list of `Fragment` objects joined by the `^` operator: ${snippet{

"this is my specification" ^
  "and example 1"          ! e1^
  "and example 2"          ! e2

def e1 = success
def e2 = success

}}

What we have here is a list of 3 fragments, a `Text` fragment and 2 `Example` fragments. The examples are declared using the format `"description" ! body` and their "bodies" are provided by 2 methods returning a `Result`, separated from the specification text.

When you are directly assembling text and examples with the `^` operator to create your specification there are a few rules which govern how it will be displayed and you can use some special "formatting" fragments to adjust the layout.

### Rules

The layout of text in ***specs2*** is mostly done automatically so that the text in the source code should look like the displayed text after execution.

By default the layout of a specification will be computed automatically based on intuitive rules:

  * when an example follows a text, it is indented
  * 2 successive examples will be at the same indentation level
  * when a text follows an example, this means that you want to describe a "subcontext", so the next examples will be indented with one more level

Let's see a standard example of this. The following fragments: ${snippet{

"this is some presentation text"      ^
  "and the first example"             ! success^
  "and the second example"            ! success

}}

will be executed and displayed as:

     this is some presentation text
     + and the first example
     + and the second example

If you specify a "subcontext", you will get one more indentation level: ${snippet{

"this is some presentation text"      ^
  "and the first example"             ! success^
  "and the second example"            ! success^
  "and in this specific context"      ^
    "one more example"                ! success

}}

will be executed and displayed as:

    this is some presentation text
    + and the first example
    + and the second example
      and in this specific context
      + one more example

### Formatting fragments

Given the rules above, you might need to use some *formatting fragments* to adjust the display

#### Separating groups of examples

The best way to separate blocks of examples is to add a blank line between them by using `p` (as in "paragraph"): ${snippet{

"this is some presentation text"      ^
  "and the first example"             ! success^
  "and the second example"            ! success^
                                      p^
"And another block of examples"       ^
  "with this example"                 ! success^
  "and that example"                  ! success

}}

This will be displayed as:

    this is some presentation text
    + and the first example
    + and the second example

    And another block of examples
    + with this example
    + and that example

That looks remarkably similar to the specification code, doesn't it? What `p` does is:

 * add a blank line (this can also be done with a simple `br`)
 * decrement the current indentation level by 1 (Otherwise the new Text would be seen as a subcontext)

#### Reset the levels

When you start having deep levels of indentation, you might need to start the next group of examples at level 0. For example, in this specification ${snippet{

"There are several options for displaying the text"      ^
  "xonly displays nothing but failures"                  ! success ^
  "there is also a color option"                         ^
    "rgb=value uses that value to color the text"        ! rgb^
    "nocolor dont color anything"                        ! nocolor^
                                                         p^
"There are different ways of hiding the text"            ^
    "by tagging the text"                                ! hideTag
// 8<--
lazy val (rgb, nocolor, hideTag) = (ok, ok, ok)
}}

Even with `p` the next group of examples will not start at level 0. What you need to do in that case is use `end`: ${snippet{

"There are several options for displaying the text"      ^
  "xonly displays nothing but failures"                  ! success^
  "there is also a color option"                         ^              // this text will be indented
    "rgb=value uses that value to color the text"        ! rgb^         // and the following examples as well
    "nocolor dont color anything"                        ! nocolor^
                                                         end^
"There are different ways of hiding the text"            ^              // this text will be properly indented now
  "by tagging the text"                                  ! hideTag^
                                                         end
// 8<--
lazy val (rgb, nocolor, hideTag) = (ok, ok, ok)
}}

This will be displayed as:

    There are several options for displaying the text
    + xonly displays nothing but failures
      there is also a color option
      + rgb=value uses that value to color the text
      + nocolor dont color anything
    There are different ways of hiding the text
    + by tagging the text

And if you want to reset the indentation level *and* add a blank line you can use `end ^ br` (or `endbr` as seen in "Combinations" below).

#### Changing the indentation level

If, for whatever reason, you wish to have more or less indentation, you can use the `t` and `bt` fragments (as in "tab" and "backtab"): ${snippet{

"this text"                                     ^ bt^
"doesn't actually have an indented example"     ! success

"this text"                                     ^ t^
    "has a very indented example"               ! success

}}

The number of indentation levels (characterized as 2 spaces on screen) can also be specified by using `t(n)` or `bt(n)`.

#### Combinations

Some formatting elements can be combined:

 * `p` is actually `br ^ bt`
 * `endbr` is `end ^ br`
 * `endp` is `end ^ p`  (same effect as `endbr` but shorter :-))

### Unit specification

Formatting fragments can be used in a unit specification as well. 2 forms are supported, either as a single declaration: ${snippet{
// 8<--
class s extends mutable.Specification { // 8<--
"this is an example" >> { 1 === 1 }
p // add a paragraph
"this is another example" >> { 2 === 2 }
// 8<--
}
}}

Or as a postfix operator on fragments: ${snippet{
// 8<--
class s extends mutable.Specification { // 8<--
"this is some text and a paragraph".p
"this is an example and a paragraph" >> {
  1 must_== 1
} p
// 8<--
}
}}

There are also 2 additional postfix operations which can be used to start new paragraphs. Instead of using `endp` to end a group of examples and start a new one: ${snippet{
// 8<--
class s extends mutable.Specification { // 8<--
"This is a first block of examples".p;
eg { 1 === 1 };
eg { 2 === 2 }; endp

"And a second block".p;
eg { 3 === 3 };
eg { 4 === 4 }
// 8<--
}
}}

You can use `newp` (or `newbr`) to the same effect: ${snippet{
// 8<--
class s extends mutable.Specification { // 8<--

"This is a first block of examples".p;
{ 1 === 1 }.eg;
{ 2 === 2 }.eg

"And a second block".p;
{ 3 === 3 }.eg;
{ 4 === 4 }.eg
// 8<--
}
}}

A shortcut is also available to indent a 'subexample' locally: ${snippet{
// 8<--
class s extends mutable.Specification { // 8<--
"this is the first major example" >> { ok }
  "this is minor and should be indented" >> { ok } lt;
"this is the second major example" >> { ok }
// 8<--
}
}}

This will output:

    this is a group of examples
    + this is the first major example
      + this is minor and should be indented
    + this is the second major example
  """

}
