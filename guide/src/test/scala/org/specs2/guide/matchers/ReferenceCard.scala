package org.specs2
package guide
package matchers

import specification.Forms

object ReferenceCard extends UserGuidePage with Forms { def is = s2"""
### Equality

${EqualityMatchers.text}

### Out of the box

These are the all the available matchers when you extend `Specification`

${ MatcherCards.toTabs }

### Optional

Those matchers are optional. To use them, you need to add a new trait to your specification:

${ OptionalMatcherCards.toTabs }

"""
}
