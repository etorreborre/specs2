package org.specs2
package guide

import org.specs2.specification.core.{SpecificationLink, SpecificationStructure}

object UserGuide extends UserGuidePage { def is = "User Guide".title ^ s2"""

 $specs2 is a library for writing executable software specifications in Scala.

 With $specs2 you can write:

 * specifications for simple classes (*unit* specifications)
 * specifications for full features (*acceptance* specifications)

  In this user guide, you will find:

 ${win}a ${ "quick start guide" ~/ QuickStart }
 ${win}how to ${"structure your specification" ~/ Structure}
 ${win}how to use ${"matchers" ~/ Matchers }
 ${win}how to ${"execute a specification" ~/ Runners }

 And much more!

 ${Contexts.hide}
 ${UseScalaCheck.hide}
 ${UseMockito.hide}
 ${Installation.hide}
 ${HowTo.hide}
 ${Troubleshooting.hide}

"""

}
