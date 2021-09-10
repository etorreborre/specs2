package org.specs2.guide

object AddKeywords extends UserGuidePage {
  def is = s2"""

Mutable specifications offer a predefined "vocabulary" to define examples: ${snippet{
import org.specs2.*

class MySpecification extends mutable.Specification:
  "the 'and' function" should {
    "return true when passed true, true" >> {
      (true && true) === true
    }
    "return false when passed true, false" >> {
      (true && false) === false
    }
  }

}}

This will print:
```
 the 'and' function should
   + return true when passed true, true
   + return false when passed true, false
```
And you can see that the word "should" has been added to the first description.

However one size does not fit all and you might want to add your own predefined words. Here is how to do it: ${snippet{
import org.specs2.*
import org.specs2.specification.core.{Fragment, Fragments}
import org.specs2.specification.dsl.mutable.*
import org.specs2.control.ImplicitParameters

trait ToKeyword extends ExtendedBlockDsl:
  extension (description: String)
    infix def to(f: =>Fragment): Fragment =
      (description + " to") >> f

    // this implementation of `to` uses an implicit parameter. This is used to overload
    // the method for different arguments: Fragment and Fragments
    infix def to(fs: =>Fragments)(using p1: ImplicitParameters.ImplicitParam1): Fragments =
      (description + " to") >> fs

class MySpecification extends org.specs2.mutable.Specification with ToKeyword:

  "the 'and' function is used" to {
    "return true when passed true, true" >> {
      (true && true) === true
    }
    "return false when passed true, false" >> {
      (true && false) === false
    }
  }
}}

Now this will print
```
 the 'and' function is used to
   + return true when passed true, true
   + return false when passed true, false
```
"""

}
