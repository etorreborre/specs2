package org.specs2
package guide

import matcher.Scope


object Scopes extends UserGuidePage { def is = s2"""

### Scope

The techniques described in ${"Context objects" ~/ ContextObjects} are not always applicable to unit specifications where we want examples to be a "block" of code described by some text. Instead of creating a case class we can instantiate a trait which will hold a "fresh" state:${snippet{
class ContextSpec extends mutable.Specification {
  "this is the first example" in new trees {
    tree.removeNodes(2, 3) must have size(2)
  }
  "this is the first example" in new trees {
    tree.removeNodes(2, 3, 4) must have size(1)
  }
}

/** the `trees` context */
trait trees extends Scope {
  val tree = new Tree(1, 2, 3, 4)
}
}}

Each example of that specification gets a new instance of the `trees` trait. So it will have a brand new `tree` variable and even if this data is mutated by an example, other examples will be isolated from these changes.

Now you might wonder why the `trees` trait is extending the `org.specs2.specification.Scope` trait? The reason is that the body of an Example only accepts objects which are convertible to a `Result`. By extending `Scope` we can take advantage of an implicit conversion provided by the `Specification` trait to convert our context object to a `Result`.

### Before / After

It is also possible to extend Scopes with `Before`, `After`, `BeforeAfter` traits but they need to be `org.specs2.mutable.Before`, `org.specs2.mutable.After` and `org.specs2.mutable.BeforeAfter` traits. This is necessary because those traits extend the Scala `DelayedInit` trait allowing to insert code around the execution of the body of an object.

$warn Do not use an abstract class instead of a trait when using `Before`, `After` or `BeforeAfter`. This will lead to the [execution of a the "delayed init" code twice](http://stackoverflow.com/questions/21154941/specs2-after-method-runs-before-the-example)!


$AndIfYouWantToKnowMore

 - "isolate" examples using the ${"`isolated` argument" ~/ Isolation}
 - print ${"execution data" ~/ PrintExecutionData}

$vid
"""

  case class Tree[T](ts: T*) {
    def removeNodes(n: Int*) = Seq[Int]()
  }

}



