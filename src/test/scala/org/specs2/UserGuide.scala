package org.specs2
import guide._
import specification._

class UserGuide extends Specification { def is = noindent^
  "User Guide".title                                                                      ^
                                                                                          """
  ***specs2*** is a library for writing executable software specifications in Scala.

  With _specs2_ you can write:

  * specifications for simple classes (you can call that *unit* specifications)
  * specifications for full features which can be used as acceptance or integration tests

  In the following user guide, you will find:
                                                                                          """^
                                                                                          end^ t^
  "a " ~ ("quick start guide", new QuickStart)                                            ^
  "how to " ~ ("structure your specification", new SpecStructure)                         ^
  "how to use " ~ ("matchers", new Matchers)                                              ^
  "how to " ~ ("execute a specification", new Runners)                                    ^
  "the ***specs2*** " ~ ("philosophy", new Philosophy)                                    ^
                                                                                          end

}