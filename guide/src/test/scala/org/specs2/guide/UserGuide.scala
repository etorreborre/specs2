package org.specs2
package guide

object UserGuide extends UserGuidePage { def is = "User Guide".title ^ s2"""

 ***specs2*** is a library for writing executable software specifications in Scala.

 With _specs2_ you can write:

 * specifications for simple classes (*unit* specifications)
 * specifications for full features (*acceptance* specifications)

  In this user guide, you will find:

  a ${ "quick start guide" ~/ QuickStart }


  how to ${"structure your specification" ~/ Structure}
  how to use ${"matchers" ~/ Matchers }
  how to ${"execute a specification" ~ Runners }
  how to ${"how to troubleshoot errors" ~/ TroubleShooting }
                                                                        """
}
