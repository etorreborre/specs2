package org.specs2
package guide

import specification.BeforeSpec

object SpecificationTemplate extends UserGuidePage { def is = s2"""

On the ${"Contexts" ~/ Contexts} page we saw that there is a way to define an action which will be executed before all examples with the `BeforeSpec` trait.
You actually might want to create your own trait extending `BeforeSpec` in order to reuse this action in more than one specification:${snippet{
trait DatabaseSetup extends BeforeSpec {
  def beforeSpec = step(println("prepare database"))
}

class DatabaseSpecification1 extends Specification with DatabaseSetup { def is = s2"""
  // do something with the database
 """
}
}}

How does this work? The `BeforeSpec` trait overrides a method called `map` in the `SpecificationStructure` trait (a parent of `Specification`)
and adds the fragments defined by `beforeSpec` before anything else in the specification:
```
override def map(fs: =>Fragments): Fragments =
  // the AlwaysTag is there to make sure that those fragments are always executed
  // even if we filter by name or other tags
  super.map(fs).prepend(beforeSpec.append(fragmentFactory.markAs(AlwaysTag)))
```

The `map` method is indeed called every time the specification returns the list of `Fragment`s defining it. You can leverage this method and define your own "Specification templates":

 - adding some text before/after a Specification
 - filtering out some undesirable fragments
 - reformatting all text
 - ...

$AndIfYouWantToKnowMore

You will probably need to learn a bit more about $specs2  ${""""Fragments" API""" ~/ FragmentsApi} in order to modify, delete or add new "Fragments".

$vid
"""
}
