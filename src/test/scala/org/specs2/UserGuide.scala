package org.specs2
import guide._
import specification._

class UserGuide extends Specification { def is =                                          
                                                                                          """
  ***specs2*** is a library for writing executable software specifications in Scala.

  With _specs2_ you can write:

  * specifications for simple classes (you can call that *unit* specifications)
  * specifications for full features which can be used as acceptance or integration tests

  In the following user guide, you will find:
                                                                                          """^
  "a " ~ ("quick start guide", new QuickStart)                                            ^
  "how to " ~ ("structure your specification", new SpecStructure)                         ^
  "how to use " ~ ("matchers", new Matchers)                                              ^
  "how to use " ~ ("mock objects", new Mocks)                                             ^
  "a tour of the " ~ ("specs2 API", new Api)                                              ^
  "the ***specs2*** " ~ ("philosophy", new Philosophy)                                    ^
                                                                                          end

}