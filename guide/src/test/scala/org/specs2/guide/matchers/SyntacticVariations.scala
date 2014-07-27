package org.specs2
package guide
package matchers

object SyntacticVariations extends UserGuidePage { def is = s2"""

The "standard" way of using matchers is `value must matcher`. However there are several allowed variations:${snippet{
// use 'should' instead of 'must'
1 should beEqualTo(1)

// use 'beXXX' and 'haveXXX' as 'be XXX' and 'have XXX'
1 must be equalTo(1)
List(1) must have size(1)

// use 'not' to negate a matcher
List(1) must not have size (1)
}}

Finally there are some shortcuts for the equality matcher:${snippet{
1 must_== 1
1 mustEqual 1
1 === 1

// with a negation
1 must_!= 2
1 mustNotEqual 1
1 !=== 1
}}
"""
}
