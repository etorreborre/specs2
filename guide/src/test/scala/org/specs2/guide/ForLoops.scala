package org.specs2
package guide

import specification.core.{Fragment, Fragments}
import execute._
import org.specs2.fp.syntax._

object ForLoops extends UserGuidePage { def is = s2"""

It is very tempting to use `foreach` to create examples or results from a sequence of values:${snippet{
  (1 to 3).foreach(i => "example "+i ! { i must_== i })
}}

The problem with `foreach` is that the return value of the expression above is `Unit`. So you won't be able to use it in an acceptance specification or a mutable one.

### A list of examples

When we want to create a list of examples we need to return a `Fragments` object. The long-winded way to do so is to use a `foldLeft`:${snippet{
  (1 to 3).foldLeft(Fragments.empty)((res, i) => res.append("example "+i ! { i must_== i }))
}}

Or, a bit fancier with Scalaz:${snippet{
  // Fragments has a Monoid so you can use the foldMap method
  (1 to 3).toList.foldMap(i => Fragments("example "+i ! { i must_== i }))
}}

Because this is a recurring pattern there are two methods encapsulating it:${snippet{
  // when the function only returns a Fragment
  Fragment.foreach(1 to 3)(i => "example "+i ! { i must_== i }): Fragments

  // when the function returns a Fragments object
  Fragments.foreach(1 to 3) { i =>
    "examples for "+i ^ br^
    "1 + "+i ! { (1 + i) must_== (i + 1) } ^ br^
    "2 + "+i ! { (2 + i) must_== (i + 2) }
  }: Fragments
}}

Now you can create a list of examples inside a "should" block in a mutable specification:${snippet{

class MySpec extends mutable.Specification {
  "this block should have lots of examples" >> {
    Fragment.foreach(1 to 1000) { i =>
      "example "+i ! { i must_== i }
    }
  }
}
}}

### A list of results

The same situation happens when you want to create a list of expectations inside an example:${snippet{
class MySpec extends mutable.Specification {
  "this example has a lot of expectations" >> {
    Result.foreach(1 to 1000) { i =>
      i must_== i
    }
  }
}
}}

In that case the `Result.foreach` method is the one to use, it returns a `Result` that is the logical `and` of all results.

$AndIfYouWantToKnowMore

 * read about the $FragmentsApi
 * understand why the `Result.foreach` method uses the $AsResultTypeclass

$vid


"""
}
