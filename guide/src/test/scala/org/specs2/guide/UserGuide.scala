package org.specs2
package guide

object UserGuide extends UserGuidePage { def is = "User Guide".title ^ s2"""

 $specs2 is a library for writing executable software specifications in Scala.

 With $specs2 you can write:

 * specifications for simple classes (*unit* specifications)
 * specifications for full features (*acceptance* specifications)

<p/>
  In this user guide, you will find:

 ${win}a ${ "quick start guide" ~/ QuickStart }
 ${win}how to ${"structure your specification" ~/ Structure}
 ${win}how to use ${"matchers" ~/ Matchers }
 ${win}how to ${"execute a specification" ~/ Runners }
<p/>
 And much more!

 ${link(QuickStart).hide}
 ${link(Structure).hide}
 ${link(Matchers).hide}
 ${link(Runners).hide}
 ${link(Contexts).hide}
 ${link(UseScalaCheck).hide}
 ${link(UseMockito).hide}
 ${link(Installation).hide}
 ${link(HowTo).hide}
 ${link(DetailedTopics).hide}
 ${link(Troubleshooting).hide}

"""

}
