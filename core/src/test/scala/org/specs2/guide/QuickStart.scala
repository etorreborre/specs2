package org.specs2
package guide

import _root_.examples._

class QuickStart extends UserGuidePage { def is = "Quick Start".title ^ s2"""
There are 2 major styles of specifications with ***specs2***:

 * _unit_ specifications where the specification text is interleaved with the specification code. It is generally used to
   specify a single class

 * _acceptance_ specifications where all the specification text stands as one and the implementation code is elsewhere.
   It is generally used for acceptance or integration scenarios

#### Unit

Unit specifications extend the `org.specs2.mutable.Specification` trait and are using the `should/in` format: ${snippet{

import org.specs2.mutable._

class HelloWorldSpec extends Specification {

  "The 'Hello world' string" should {
    "contain 11 characters" in {
      "Hello world" must have size(11)
    }
    "start with 'Hello'" in {
      "Hello world" must startWith("Hello")
    }
    "end with 'world'" in {
      "Hello world" must endWith("world")
    }
  }
}
}}

#### Acceptance

Acceptance specifications extend the `org.specs2.Specification` trait and must define a method called `is`: ${snippet{

import org.specs2._

class HelloWorldSpec extends Specification { def is = s2"""

 This is a specification to check the 'Hello world' string

 The 'Hello world' string should
   contain 11 characters                                         $e1
   start with 'Hello'                                            $e2
   end with 'world'                                              $e3
                                                                 """

  def e1 = "Hello world" must have size(11)
  def e2 = "Hello world" must startWith("Hello")
  def e3 = "Hello world" must endWith("world")
}
}}

The **`is`** method lists [*specification fragments*](org.specs2.guide.Structure.html#Declare+examples) which can be interpolated from a **`s2`** String where you can inject:

* examples: some executable code returning a [`Result`](org.specs2.guide.Matchers.html)
* [ScalaCheck](org.specs2.guide.Matchers.html#ScalaCheck) properties
* [Forms](org.specs2.guide.FormsPage.html)
* normal strings


#### Execution

And this is it! Now to execute your specification, you use a [*runner*](org.specs2.guide.Runners.html#Presentation) which will display the results:

    > scala -cp ... specs2.run HelloWorldSpec

    HelloWorldSpec

    This is a specification to check the 'Hello world' string

    The 'Hello world' string should
    + contain 11 characters
    + start with 'Hello'
    + end with 'world'

    Total for specification HelloWorldSpec
    Finished in 0 second, 58 ms
    3 examples, 0 failure, 0 error

#### And much more!

You can explore the rest of this [User Guide](org.specs2.guide.UserGuide.html "Guide") to learn how to:

 * use the many ***specs2*** matchers to specify precise expectations
 * define _contexts_ to setup/teardown data for your examples
 * include / link specifications and reuse examples
 * use Mockito or ScalaCheck
 * use sbt/maven/junit to execute a specification
 * export your specification as an html document (like this one!)
  """^
  include(xonly, new HelloWorldSpec)                                                                                    ^
  include(xonly, new HelloWorldUnitSpec)                                                                                ^
                                                                                                                        end

}
