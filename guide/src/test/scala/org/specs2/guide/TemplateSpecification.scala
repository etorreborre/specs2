package org.specs2
package guide

import specification.BeforeAll

object TemplateSpecification extends UserGuidePage { def is = s2"""

On the ${"Contexts" ~/ Contexts} page we saw that there is a way to define an action which will be executed before all examples with the `BeforeAll` trait. It is very likely that that you will actually make your own trait extending `BeforeAll` to reuse this action in more than one specification:${snippet{
trait DatabaseSetup extends BeforeAll {
  def beforeAll = println("prepare database")
}

class DatabaseSpecification1 extends Specification with DatabaseSetup { def is = s2"""
  // do something with the database
 """
}
}}

How does this work? The `BeforeAll` trait overrides a method called `map` in the `Specification` trait and adds one `Step` before anything else in the specification:
```
override def map(fs: =>Fragments): Fragments =
  super.map(fs).prepend(fragmentFactory.step(beforeAll))
```

The `map` method is indeed called everytime the specification returns the list of `Fragment` defining it. You can leverage this and define your own traits:

 - adding some text before a Specification
 - filtering out some undesirable fragments
 - reformatting all text
 - ...

### If you want to know more

You will probably need to learn a bit more about $specs2 ${""""Fragments" API""" ~/ FragmentsApi} in order to modify, delete or add new "Fragments".

"""
}

