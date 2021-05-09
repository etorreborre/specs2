package org.specs2
package guide

object QuickStart extends UserGuidePage { def is = "Quick Start".title ^s2"""

Follow the ${"installation" ~/ Installation} instructions and create the following specification in a file named `HelloWorldSpec.scala`: ${snippet{

import org.specs2.*

class HelloWorldSpec extends Specification:
  def is = s2"""

This is a specification to check the 'Hello world' string

The 'Hello world' string should
  contain 11 characters $e1
  start with 'Hello' $e2
  end with 'world' $e3

  """

  def e1 = "Hello world" must haveSize(11)
  def e2 = "Hello world" must startWith("Hello")
  def e3 = "Hello world" must endWith("world")

}}

A $specs2 software specification is a Scala class extending `org.specs2.Specification` and declaring an `is` method.
That method defines a `s2` interpolated string with some plain text describing what the system should do
and some code with executable examples.

#### Unit specifications

The style of writing specifications above, with most of the text first, then executable examples, is unconventional.
You can, if you prefer, use an alternative style: ${snippet{

// note the different import here
import org.specs2.mutable.*

class HelloWorldSpec extends Specification:

  "This is a specification to check the 'Hello world' string".br

  "The 'Hello world' string should" >> {
    "contain 11 characters" >> {
      "Hello world" must haveSize(11)
    }

    "start with 'Hello'" >> {
      "Hello world" must startWith("Hello")
    }

    "end with 'world'" >> {
      "Hello world" must endWith("world")
    }
  }
}}

Both specifications will produce the same output.

### Execution

And this is it! Now you can execute your specification with a [*runner*](org.specs2.guide.Runners.html#Presentation) and observe the results:

```
sbt> testOnly *HelloWorldSpec

[info] HelloWorldSpec
[info]
[info] This is a specification to check the 'Hello world' string
[info]
[info] The 'Hello world' string should
[info]   + contain 11 characters
[info]   + start with 'Hello'
[info]   + end with 'world'
[info]
[info] Total for specification HelloWorldSpec
[info] Finished in 0 second, 58 ms
[info] 3 examples, 0 failure, 0 error
```

${h3Ribbon("Learn more!")}

The rest of this ${see(UserGuide)} will show you how to:

 * ${"structure" ~/ Structure} your specification using one of the 2 major "styles" of specifications presented here: "Acceptance" and "Unit"
 * use the many $specs2 ${"matchers" ~/ Matchers} to specify precisely the expected behavior of your application
 * ${"run" ~/ Runners} your specification and output results in various formats
 * check out the ${"How to?" ~/ HowTo} page to find an answer to a specific question

$vid
"""

}
