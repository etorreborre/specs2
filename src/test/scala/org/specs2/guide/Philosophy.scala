package org.specs2
package guide

class Philosophy extends Specification { def is = freetext                                                              ^
                                                                                                                        """
### The origins

***specs2*** has been created as an evolution of the [***specs***](http://code.google.com/p/specs) project.

***specs*** started as learning project to explore Scala's DSL possibilities for doing Behaviour-Driven-Development (BDD).
Among the very first objectives of specs were:

 * conciseness: it should be possible to express with the least amount of ceremony a software specification
 * readability: an _executable_ specification should read like a purely textual one, by a non-programer
 * extensibility: it should be possible to add new matchers, new runners,...
 * clear implementation: since this is an open-source project with no business constraint,
   the author should have ample time to refine the implementation until it's crystal clear
 * great user support: it's not because something is free that it should be buggy and this is also a good test on the
   design. A good design should be easy to fix and evolve
                                                                                                                        """^
                                                                                                                        """
### The score

After a few years of use, let's have a look at what worked and what didn't.
##### Conciseness

This objective was achieved thanks to the incredible power of implicits in Scala which provide lots of way to create
an elegant syntax for specifying software. The following points must however be noted:

  *  ***specs*** provides implicits by inheriting methods from the `Specification` class. While this is very convenient
     this is also considerably polluting the namespace of the user code _inside_ the specification

  *  some "reserved" words like `should`, `can`, `in` come from previous BDD libraries like rspec. But they are not always
     convenient and sometimes one wants to write `"my example should provide"` just to avoid having every example under
    `"my example" should` start with `"provide..."`.
     There is a way to do this in ***specs*** but this requires the creation of an ad-hoc method

  *  some people like to structure their specifications with other keywords like Given-When-Then which requires
     additional methods in the library for no more added value than just displaying those words

                                                                                                                        """^
                                                                                                                        end

}