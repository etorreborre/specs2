package org.specs2
package guide

class UserGuide extends UserGuidePage { def is = "User Guide".title ^ s2"""

 ***specs2*** is a library for writing executable software specifications in Scala.

 With _specs2_ you can write:

 * specifications for simple classes (*unit* specifications)
 * specifications for full features (*acceptance* specifications)

  In this user guide, you will find:

  ${ "a " ~ ("quick start guide", new QuickStart)                       }
  ${ "how to " ~ ("structure your specification", new Structure)        }
  ${ "how to use " ~ ("matchers", new Matchers)                         }
  ${ "how to " ~ ("execute a specification", new Runners)               }
  ${ "\"how to\" tips " ~ ("to improve your specifications", new HowTo) }
  ${ "the ***specs2*** " ~ ("philosophy", new Philosophy)               }
  ${ "the ***specs2*** " ~ ("design", new Design)                       }
                                                                        """
}
