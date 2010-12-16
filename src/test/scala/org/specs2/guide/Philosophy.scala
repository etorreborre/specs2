package org.specs2
package guide

class Philosophy extends Specification { def is = freetext                                                              ^
                                                                                                                        """
### The origins

***specs2*** has been created as an evolution of the [***specs***](http://code.google.com/p/specs) project.

***specs*** started as learning project to explore Scala's DSL possibilities for doing Behaviour-Driven-Development (BDD).
Among the very first objectives of specs were:

 * conciseness: it should be possible to express with the least amount of ceremony a software specification

 * readability: an _executable_ specification should read like a purely textual one, by a non-programmer

 * extensibility: it should be possible to add new matchers, new runners,...

 * configuration: there should sensible defaults and an easy way to override those values

 * clear implementation: since this is an open-source project with no business constraint, I should have ample time to
   refine the implementation until it's crystal clear, right?

 * great user support: it's not because something is free that it should be buggy! Moreover this is also a good test on the
   design. A good design should be easy to fix and evolve
                                                                                                                        """^
                                                                                                                        """
### The score

After a few years of use, let's have a look at what worked and what didn't.

###### Conciseness

This objective was achieved thanks to the incredible power of implicits in Scala which provide lots of way to create
an elegant syntax for specifying software. The following points must however be noted:

  *  ***specs*** provides implicits by inheriting methods from the `Specification` class. While this is very convenient
     this is also considerably polluting the namespace of the user code _inside_ the specification. This pollution leads
     to possible bugs (an implicit conversion being applied when you don't expect it) and possible namespace conflicts

  *  some "reserved" words like `should`, `can`, `in` come from previous BDD libraries like rspec. But they are not always
     convenient and sometimes one wants to write `"my example should provide"` just to avoid having every example under
    `"my example" should` start with `"provide..."`.
     There is a way to do this in ***specs*** but this requires the creation of an ad-hoc method

  *  some people like to structure their specifications with other keywords like Given-When-Then which require
     additional methods in the library for no more added value than just displaying those words

###### Readability

The readability of a specification written with ***specs*** largely depends on the writer of the specification.
Since a specification is created by interleaving text and code, if the amount of code is too large then the
textual content of the specification is largely lost and the developer cannot read it with just one glance.

Something was prototyped in ***specs*** to alleviate this issue: [LiterateSpecifications](http://code.google.com/p/specs/wiki/LiterateSpecifications#A_short_example).
The idea was to use Scala support for XML literal to allow the writer to write pure text and insert, at the right places,
some executable examples. It turns out that the implementation of this approach is fairly different from what was done for
"regular" examples and cluttered the overall design.

###### Extensibility

Since its first release, ***specs*** has been extended in different ways. Many users have created their own matchers ([some
of them](http://code.google.com/p/specs/wiki/MatchersGuide#Eventually_matchers) have been integrated to the main library).

On the other hand, the additional runners which were developed for TeamCity or sbt were written by me, as well as other
more fundamental enhancements, like [SpecificationContexts](http://code.google.com/p/specs/wiki/DeclareSpecifications#Specification_context).

The conclusion we can draw on this subject is that it's difficult to design something that's truly extensible without any
idea of the future requirements!

###### Configuration

The "sensible" defaults have been debated and some "opiniated" decisions have been taken, like the decision to mark
any example without expectation as "Pending" for example. I must say that the Configuration mechanism offered in
exchange was not really appealing (a mix of properties-file-reflection-system-properties) and there is few evidence that
anyone actually used it.

###### Clear implementation

This objective was certainly *not* achieved. There are several reasons for this. The design of the examples execution is
certainly the main one. In ***specs*** the examples are executed "on-demand" when a runner "asks" the specification for
its successes and failures. The specification then asks each example for its status and an example knows that by executing
himself. The trouble is, the example doesn't really enough information to know it's full execution context. Do we have
some code to executed beforehand for data setup? Or afterwards, because it's the last example of the specification and
a database disconnection is required then?

This design is aggravated by a "magic" feature provided by ***specs***: [the automatic reset of local variables](http://code.google.com/p/specs/wiki/DeclareSpecifications#Execution_model).
This feature is _very_ convenient for the user but the implementation is a nightmare! In order to make as if each example
was executed in isolation, as if other examples did not modify surrounding variables, a new specification is created and
just one example is executed inside that specification. This works, but at the expense of carefully copying a lot of state
from the cloned specification to the original one. More than *20* issues were created because of that only feature!

###### User support

The user support has always been responsive, in terms of bug fixing and enhancements. However the object-oriented nature
of ***specs*** with a lot of variables and side-effects around made some bugs difficult to diagnose and render some enhancements
downright impossible. The best example of an "impossible" feature to implement is the concurrent execution of examples.
With shared variables all around the place, there's little chance to ever get it right.

### A new compromise

The heart of ***specs2*** is a brand new set of design rules, forming a new compromise:

 1. Functional orientation / no mutable variables

 2. There is no explicit structure

 3. The default Specification style should encourage the readability of the full specification text, without
    having too much interleaved code

 4. Be mostly on par with the existing features and adding new powerful ones

 5. Control the dependencies (no cycles)

 6. Control the implicits scopes

##### Functional / immutable

####### Chaining everything

Mutable variables were the subject of enough grief, I decided it was high time to do without them. This decision has a
big impact on the user. A specification can not anymore be a set of unrelated "blocks" where each block is added to the
parent specification through a side effect:

      "my example is ok" in { 1 must_== 1 }
      "my other example is ok" in { 2 must_== 2 }

Now the "blocks" have to form a sequence:

      "my example is ok"       ! e1^ // notice the ^ operator here
      "my other example is ok" ! e2

      def e1 = { 1 must_== 1 }
      def e2 = { 2 must_== 2 }

The same thing applies to an Example body and has a major consequence: you have to explicitly chain expectations!

      "my example on strings" ! e1  // will never fail!

      def e1 = {
        "hello" must have size(10000)  // because this expectation will not be returned,...
        "hello" must startWith("hell")
      }

      // the correct way of writing the example is

      "my example on strings" ! e1  // will fail

      def e1 = "hello" must have size(10000) and must startWith("hell")

####### No structure

This principal comes from a new approach on "nesting" and "composition". In

                                                                                                                        """^
                                                                                                                        end

      val examples = new Specification { def is =

        "my example on strings" ! e1  // will fail
        def e1 = "hello" must have size(10000) and must startWith("hell")
      }
}