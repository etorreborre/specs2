package org.specs2
import guide._

class UserGuide extends Specification { def is = 
  "User Guide".title                                                                             ^
                                                                                                 """
  ***specs2*** is a library for writing executable software specifications in Scala.

  With _specs2_ you can write:

  * specifications for simple classes (*unit* specifications)
  * specifications for full features (*acceptance* specifications)

  In this user guide, you will find:                                                             """^
                                                                                                 p^
  "a " ~ ("quick start guide", new QuickStart)                                                   ^
  "how to " ~ ("structure your specification", new Structure)                                ^
  "how to use " ~ ("matchers", new Matchers)                                                     ^
  "how to " ~ ("execute a specification", new Runners)                                           ^
  "the ***specs2*** " ~ ("philosophy", new Philosophy)                                           ^
  "the ***specs2*** " ~ ("design", new Design)                                                   ^
                                                                                                 end

}