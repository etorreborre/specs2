package org.specs2
package guide

import specification.core.{Fragment, Fragments}
import execute.*
import org.specs2.fp.syntax.*

object ForLoops extends UserGuidePage { def is = s2"""

It is very tempting to use `foreach` to create examples or results from a sequence of values:${snippet{
  (1 to 3).foreach(i => "example "+i ! { i must ===(i) })
}}

The problem with `foreach` is that the return value of the expression above is `Unit`. So you won't be able to use it in an acceptance specification or a mutable one.

### A list of examples

When we want to create a list of examples we need to return a `Fragments` object. The long-winded way to do so is to use a `foldLeft`:${snippet{
  (1 to 3).foldLeft(Fragments.empty)((res, i) => res.append("example "+i ! { i must ===(i) }))
}}

Or, a bit fancier with `foldMap`:${snippet{
  // Fragments has a Monoid instance so you can use the foldMap method
  (1 to 3).toList.foldMap(i => Fragments("example "+i ! { i must ===(i) }))
}}

Because this is a recurring pattern there are two methods encapsulating it:${snippet{
  // when the function only returns a Fragment
  Fragment.foreach(1 to 3)(i => "example "+i ! { i must ===(i) }): Fragments

  // when the function returns a Fragments object
  Fragments.foreach(1 to 3) { i =>
    "examples for "+i ^ br^
    "1 + "+i ! { (1 + i) must ===((i + 1)) } ^ br^
    "2 + "+i ! { (2 + i) must ===((i + 2)) }
  }: Fragments
}}

Now you can create a list of examples inside a "should" block in a mutable specification:${snippet{

class MySpec extends mutable.Specification {
  "this block should have lots of examples" >> {
    Fragment.foreach(1 to 1000) { i =>
      "example "+i ! { i must ===(i) }
    }
  }
}
}}

### A list of expectations

Similarly, when you want to create a list of expectations inside an example, you should use a variant of `foreach` and `forall` methods:

 - If you need "thrown expectations", use the `foreach` and `forall` methods of `mutable.Specification`:

${snippet{
class MySpec extends mutable.Specification {
  "this collects results of all expectations and throws an exception" >> {
    foreach(1 to 10) { i =>
      i must_== 2
    } // Collects results of all expectations. Throws an exception.
    foreach(1 to 10) { i =>
      i === i
    } // This is not executed.
  }
  "this stops after the first failed expectation and throws an exception" >> {
    forall(1 to 10) { i =>
      i === 2
    } // Stops after the first failed expectation. Throws an exception.
  }
}
}}

 - If you need "functional expectations" that return a `Result`, use `Result.foreach` or `Result.forall`:

${snippet{
class MySpec extends mutable.Specification {
  "this collects results of all expectations and returns a Result" >> {
    Result.forall(1 to 10) { i =>
      i === 2
    }
  }
  "this stops after the first failed expectation and returns a Result" >> {
    Result.foreach(1 to 10) { i =>
      i === 2
    }
  }
}
}}

$AndIfYouWantToKnowMore

 * read about the $FragmentsApi
 * understand why the `Result.foreach` method uses the $AsResultTypeclass

$vid
"""
}
