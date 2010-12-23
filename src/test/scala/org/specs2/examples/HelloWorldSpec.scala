package org.specs2
package examples

class HelloWorldSpec extends Specification { def is =

  "This is a specification to check the 'Hello world' string"                    ^
                                                                                 p^
  "'Hello world' contains 11 characters"                                         ! e1^
  "'Hello world' starts with 'Hello'"                                            ! e2^
  "'Hello world' ends with 'world'"                                              ! e3^
                                                                                 end
  def e1 = "Hello world" must have size(11)
  def e2 = "Hello world" must startWith("Hello")
  def e3 = "Hello world" must endWith("world")
}
