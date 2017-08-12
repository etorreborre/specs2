package org.specs2
package guide

object ReferenceOtherSpecifications extends UserGuidePage { def is = s2"""

For some large projects, or to write documentation, you will need to structure your specifications so that some of them will reference others. Those references will be of 2 types:

 - "see" reference: a simple textual reference, with an html link to navigate to the other specification when you create an html report
 - "link" reference: an "executed" reference where the second specification will be executed and its status reported in the first one

Here is the DSL you will use for those 2 types of references:${snippet{
object FirstSpecification extends Specification { def is = s2"""
 We can consider one example
  ${ 1 must_== 1 }

  And all these examples are also important so we need to know if they all pass
  ${"important specification" ~ SecondSpecification}

  Finally it is worth having a look at ${"this specification" ~/ ThirdSpecification}.
"""
}

import org.specs2.specification.core._

object SecondSpecification extends Specification { def is = s2"""
 This spec contains lots of examples
   ${ Fragment.foreach(1 to 100) { i => "example "+i ! ok } }
"""
}
object ThirdSpecification extends Specification { def is = s2"""
 This is the third specification with a simple example
   this should pass $ok
"""
}

}}

The syntax shown above to create references is using a string for the link alias and uses two operators:

 Operator  | Description
 --------- | -----------
 `~`       | a *`link` reference*. The referenced specification gets executed when the first one is
 `~/`      | a *`see` reference*. The referenced specification doesn't get executed (`"$$FirstSpecification"` creates a *see* link as well)

Also, for better html rendering, you can add a tooltip:${snippet{
// 8<--
object OtherSpec extends Specification { def is = """nothing""" }
class s extends Specification { def is = s2""" // 8<--
  ${ "alias" ~/ (OtherSpec, "tooltip") }
// 8<--
"""
}
}}

Finally I'm also drawing your attention to the fact that you don't have to create your specifications as Scala classes but you can use simple objects as shown above.

### Reporting

By default specification links are reported with a status icon in HTML pages. You can change this by using the following methods:

 - `link(MySpec).hide` doesn't show the link at all. This is useful when you want a children specification to be executed from a parent one without having to mention it
 - `link(MySpec).mute` doesn't show the link status. This is useful when you want a children specification to be executed from a parent one but just display its html link

### Execution

When you execute a given specification you can pass the `all` argument to execute all the referenced specifications. They will be collected and executed in (topological sort)[http://en.wikipedia.org/wiki/Topological_sorting] order and any cycle in the reference graph will be broken. Only the "link" references will be executed, not the "see" references.

"""
}
