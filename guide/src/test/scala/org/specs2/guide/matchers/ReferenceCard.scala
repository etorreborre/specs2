package org.specs2
package guide
package matchers

object ReferenceCard extends UserGuidePage { def is = s2"""
## Equality

${EqualityMatchers.text}

## Out of the box

${ MatcherCards.toTabs }

## Optional

${ OptionalMatcherCards.toTabs }

"""
}
