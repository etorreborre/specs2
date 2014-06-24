package org.specs2
package guide

import scala.collection.mutable.ListBuffer

object NamedExamples extends UserGuidePage { def is = s2"""

### Naming examples

When you create acceptance specifications, you have to find names to reference your examples and this can sometimes be a bit tedious. So why not have the specification do it for you? With the `${fullName[specification.Grouped]}` trait you get "group traits", from `g1` to `g22` to define groups of examples. Each group trait defines 22 variables named `e1` to `e22`, to define examples. This is an example on how to use groups: ${snippet{
class MySpecification extends Specification with specification.Grouped { def is =  s2"""
  first example in first group                                        ${g1.e1}
  second example in first group                                       ${g1.e2}

  first example in second group                                       ${g2.e1}
  second example in second group                                      ${g2.e2}
  third example in second group, not yet implemented                  ${g2.e3}
  """
  // group of examples with no description
  new g1 {
    e1 := ok
    e2 := ok
  }
  // group of examples with a description for the group
  "second group of examples" - new g2 {
    e1 := ok
    e2 := ok
  }
}
}}

One cool feature is that with groups you can use the example names right away with no implementation. For example if you write `this is an example $${g2.e3}`, without providing an implementation, the example will be marked as `Pending`.

### Grouped vs Groups

The groups from the `Grouped` trait can hold variables which you can use in your examples. However you must be careful because those variables will be shared across all the examples of a group. You can avoid this by using the `Groups` trait like this: ${snippet{
class MySpecification extends Specification with specification.Groups { def is =  s2"""
  first example    ${g1().e1}
  second example   ${g1().e2}
  """

  "first group" - new g1 {
    val list: ListBuffer[Int] = new scala.collection.mutable.ListBuffer[Int]

    e1 := {
      list += 1
      list must haveSize(1)
    }

    // the second example is isolated from the first
    // and will succeed
    e2 := {
      list.append(1, 2, 3)
      list must haveSize(3)
    }
  }

}
}}


"""
}
