package org.specs2
package guide
package structure

import examples._

object Links extends UserGuidePage { def is = ""
  def section = s2"""

### Links

There are 2 ways to "link" specifications:

 * by including another specification, to create a parent-child relationship
 * by creating a reference to another specification, to create a peer relationship

#### Inclusion

There is a simple mechanism for including a "children" specification in a given specification. You can simply add the child specification as if it was a simple fragment: ${snippet{

s2"""
This is an included specification
  $childSpec
"""
}}

Otherwise, if you want to include several specifications at once you can use the `include` method: ${snippet{

s2"""
These are the included specifications
  ${include(childSpec1, childSpec2, childSpec3)}
"""
}}

The effect of doing so is that all the fragments of the children specification will be inlined in the parent one. This is exactly what is done in some pages of this user guide, but with a twist: ${snippet{

s2"""
  ${include(xonly, new HelloWorldSpec) }
  ${include(xonly, new HelloWorldUnitSpec)}
"""
}}

In the code above there are specific arguments to the included specifications so that they are only displayed when there are failures.

##### Inline

When you include a specification in another one the console will display the beginning and end statistics of the included specification. If you just want to insert the "middle" fragments of the included specification you can use `inline`: ${snippet{
s2"""
  ${inline(otherSpecification)}
"""
}}
##### Html link

In order to create a User Guide such as this one, you might want the included specification to be written to another html file. In this case, you need a "Link": ${snippet{
s2"""
  ${link(new QuickStart)}
"""
}}
This declaration will include the child specification so it is executed when the parent specification is executed. However during the reporting, only a Html link will be created in the parent file, referencing a separate file for the children specification. On the other hand if you "hide" the specification, the link will not be printed out: ${snippet{
s2"""
  ${link((new QuickStart).hide)}
 """
}}
###### Html Link

It is possible to customize the generated Html link with the following syntax: ${snippet{
// 8<--
class s extends Specification { def is = s2""" // 8<--
${"a " ~ (new QuickStart, "for you")}
// 8<--
"""
}
}}

The `~` operator is used to create a `HtmlLink` where:

* "a" is the beginning of the text
* the `QuickStart` specification title is the text that will be highlighted as a html link
* `new QuickStart` is the specification to include, the url being derived from the specification class name (or user-defined if you defined the specification title with an additional `urlIs`)

Several variations are possible on this pattern, depending which part of the link you want to be highlighted: ${snippet{
// 8<--
class s extends Specification { def is = s2""" // 8<--
  ${"before text" ~ (specification, "after text")                                      }
  ${"before text" ~ ("other text to highlight", specification, "after text", "tooltip")}
  ${"text to highlight" ~ specification                                                }
  ${"text to highlight" ~ (specification, "after text")                                }
  ${"text to highlight" ~ (specification, "after text", "tooltip")                     }
// 8<--
  """
}
}}

#### Reference

Sometimes you just want to reference another specification without triggering its execution. For example when [creating an index page](#Create+an+index): ${snippet{
// 8<--
s2""" // 8<--
${see(new MailSenderSpec)}
// 8<--
"""
}}

This will generate a html link in the main specification based on the referenced specification name. If you want to customize that link you can use the following syntax: ${snippet{
// 8<--
class s extends Specification { def is = s2""" // 8<--
  ${"before text" ~/ ("text to highlight", specification, "after text")           }
  ${"before text" ~/ ("text to highlight", specification, "after text", "tooltip")}
  ${"text to highlight" ~/ specification                                          }
  ${"text to highlight" ~/ (specification, "after text")                          }
  ${"text to highlight" ~/ (specification, "after text", "tooltip")               }
// 8<--
  """
}
}}

#### Markdown url

If you just want to reference the url of the html page that's being generated for a given specification in a paragraph of text, you can use the `${termName(specification.markdownLink)}` method: ${snippet{
s"""
  For more information you can read ${DetailedSpec.markdownLink}
  // or
  For more information you can read ${DetailedSpec.markdownLink("the detailed specification")}
  // or
  For more information you can read ${"the detailed specification".markdownLink(DetailedSpec)}
"""
}}
  """
  object specification extends Specification { def is = "" }
  class MailSenderSpec extends Specification { def is = "" }
  lazy val (childSpec, childSpec1, childSpec2, childSpec3, otherSpecification) =
    (specification, specification, specification, specification, specification)
  lazy val DetailedSpec = specification
}
