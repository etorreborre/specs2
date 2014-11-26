package org.specs2
package guide

object QuickStart extends UserGuidePage { def is = "Quick Start".title ^s2"""

Follow the ${"installation" ~/ Installation} instructions and create the following specification in a file named `HelloWorldSpec.scala`: ${snippet{

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

A $specs2 software specification is nothing more than a Scala class extending `org.specs2.Specification` and declaring a special `is` method. This method contains both plain text describing what the application should do but also some method calls containing a more formal specification of the expected behaviour.

### Execution

And this is it! Now you can execute your specification with a [*runner*](org.specs2.guide.Runners.html#Presentation) and observe the results:

```
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
```

${h3Ribbon("Learn more!")}

The rest of this ${see(UserGuide)} will show you how to:

 * ${"structure" ~/ Structure} your specification using one of the 2 major "styles" of specifications: "Acceptance" and "Unit"
 * use the many $specs2 ${"matchers" ~/ Matchers} to specify precisely the expected behavior of your application
 * ${"run" ~/ Runners} your specification and output results in various formats
 * check out the ${"How to?" ~/ HowTo} page to find an answer to a specific question

$vid
"""

}
