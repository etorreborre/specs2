package org.specs2
package guide

class Philosophy extends UserGuidePage { def is =
  """
### The origins

***specs2*** has been created as an evolution of the [***specs***](http://code.google.com/p/specs) project.

***specs*** started as learning project to explore Scala's DSL possibilities for doing Behaviour-Driven-Development (BDD).
Among the very first objectives of specs were:

 * **conciseness**: it should be possible to express a software specification with the least amount of ceremony

 * **readability**: an _executable_ specification should read like a purely textual one, by a non-programmer

 * **extensibility**: it should be possible to add new matchers, new runners,...

 * **configuration**: there should sensible defaults and an easy way to override those values

 * **clear implementation**: since this is an open-source project with no business constraint, there's no excuse for not having a crystal clear implementation, right?

 * **great user support**: it's not because something is free that it should be buggy! Moreover this is also a good test on the design. A good design should be easy to fix and evolve

  """^
  raw"""
### The score

After a few years of use, let's have a look at what worked and what didn't.

###### Conciseness

This objective was achieved thanks to the incredible power of implicits in Scala which provide lots of way to create an elegant syntax for specifying software. However, the implementation of that syntactic support has several drawbacks:

  *  ***specs*** provides implicits by inheriting methods from the `Specification` class. While this is very convenient this is also considerably polluting the namespace of the user code _inside_ the specification. This pollution leads to possible bugs (an implicit conversion being applied when you don't expect it) and possible namespace conflicts

  *  some "reserved" words like `should`, `can`, `in` come from previous BDD libraries like rspec. But they are not always convenient and sometimes one wants to write `"my example should provide"` just to avoid having every example under "`"my example" should`" start with `"provide..."`. There is a way to do this in ***specs*** but this requires the creation of an ad-hoc method

  *  some people like to structure their specifications with other keywords like Given-When-Then which require additional methods in the library for no more added value than just displaying those words

###### Readability

The readability of a specification written with ***specs*** largely depends on the writer of the specification. Since a specification is created by interleaving text and code, if the amount of code is too large then the textual content of the specification is largely lost and the developer cannot read it with just one glance.

Something was prototyped in ***specs*** to alleviate this issue: [LiterateSpecifications](http://code.google.com/p/specs/wiki/LiterateSpecifications#A_short_example). The idea was to use Scala support for XML literals in order to allow the writer to write pure text and insert, at the right places, some executable examples. It turns out that the implementation of this approach is fairly different from what was done for "regular" examples and cluttered the overall design.

###### Extensibility

Since its first release, ***specs*** has been extended in different ways. Many users have created their own matchers ([some of them](http://code.google.com/p/specs/wiki/MatchersGuide#Eventually_matchers) have been integrated to the main library).

On the other hand, the additional runners which were developed for TeamCity or sbt were written by me, as well as other more fundamental enhancements, like [SpecificationContexts](http://code.google.com/p/specs/wiki/DeclareSpecifications#Specification_context).

The conclusion we can draw on this subject is that it's difficult to design something that's truly extensible without any idea of the future requirements!

###### Configuration

The "sensible" defaults have been debated and some "opinionated" decisions have been taken, like the decision to mark any example without expectation as "Pending" for example. Unfortunately the configuration mechanism offered in exchange (to change the defaults) was not really appealing (a mix of properties-file-reflection-system-properties) and there is few evidence that anyone actually used it.

###### Clear implementation

This objective was certainly *not* achieved. There are several reasons for this. The design of the examples execution is certainly the main one.

In ***specs*** the examples are executed "on-demand" when a runner "asks" the specification for its successes and failures. The specification then asks each example for its status and an example knows that by executing himself. The trouble is, the example doesn't really have enough information to know it's full execution context: is there some code to execute beforehand for data setup? Or after all other examples, because it's the last example of the specification and a database disconnection is required then?

This design is aggravated by a "magic" feature provided by ***specs***: [the automatic reset of local variables](http://code.google.com/p/specs/wiki/DeclareSpecifications#Execution_model).
This feature is _very_ convenient for the user but the implementation is a nightmare! In order to make as if each example was executed in isolation, as if other examples did not modify surrounding variables, a new specification is created and just one example is executed inside that specification. This works, but at the expense of carefully copying a lot of state from the cloned specification to the original one. More than *20* issues were created because of that only feature!

###### User support

The user support has always been responsive, in terms of bug fixes and enhancements. However the object-oriented nature of ***specs*** with lots of variables and side-effects around made some bugs difficult to diagnose and made some enhancements downright impossible. The best example of an "impossible" feature to implement is the concurrent execution of examples.
With shared variables all around the place, there's little chance to ever get it right.

### A new hope

The redesign of ***specs2*** was precisely started to fight the complexities and issues of ***specs***. In order to do that while remaining true to the original vision for ***specs***, a new design compromise was necessary with new design principles:

 1. Do not use mutable variables!
 2. Use a simple structure
 3. Control the dependencies (no cycles)
 4. Control the implicits scopes

As we will see in the paragraphs below, this is a compromise in the sense that there is a bit more burden on the developer who has to write more code to get his specification into shape.

##### Functional / immutable

###### Chaining everything

Mutable variables were the subject of enough grief, I decided it was high time to do without them. This decision has a big impact on the way a user writes a specification. A specification can not anymore be a set of unrelated "blocks" where each block is added to the parent specification through a side effect:

    "my example is ok" in { 1 must_== 1 }        // those examples are added to the specification by mutating a
    "my other example is ok" in { 2 must_== 2 }  // variable

Now the "blocks" have to form a sequence:

    "my example is ok"       ! e1^               // notice the ^ operator here
    "my other example is ok" ! e2

    def e1 = { 1 must_== 1 }
    def e2 = { 2 must_== 2 }

This is clearly a drawback of not having side-effects. The presence of `^` everywhere produces unwanted syntactic noise in the specification. One way of minimizing that noise is to make good use of an editor with column editing and align those symbols on the print margin of the screen. The specification can then be read as having 2 columns, one with the text, one with the implementation and the formatting directives.

The same principle applies to the Examples bodies and has a major consequence: you have to explicitly chain expectations!

    "my example on strings" ! e1                // will never fail!
    def e1 = {
      "hello" must have size(10000)             // because this expectation will not be returned,...
      "hello" must startWith("hell")
    }

    // the correct way of writing the example is
    "my example on strings" ! e1               // will fail
    def e1 = "hello" must have size(10000) and
                          startWith("hell")

This can be seen as a limitation as well but also as an opportunity for writing better specifications. It's been indeed advocated in several places that there should be only one expectation per example, now the design of ***specs2*** actually encourages it!

One other positive consequence of that design decision is that debugging the library should be almost brainless. Functional Programming is like having a pipe-line. If you don't like the output, you just cut the pipeline in smaller pieces, examine the ins and outs of each and decide where things went wrong.

That being said, based on early feedback, a _mutable_ version of the Specification trait was later introduced in order to provide a nicer DSL for _unit_ specifications, where the code is interleaved with the descriptions (see [below](org.specs2.guide.Philosophy.html#But+if+you+STILL+want+mutable+specifications)). The mutability is limited to **1** variable and to the construction phase of the specification.

###### Arguments have to be supplied

The "local configuration" of a Specification in ***specs*** is realized with side-effects too. If you want to declare that the examples in a specification will share variables you can add `shareVariables()` at the top of the specification.

This is not possible anymore in specs2, so you have to explicitly pass arguments at the top of your specification and chain them with the rest:

```
new Specification { def is =  args(color=false)   ^ // will not output colors
  "the rest of the specs"                         ^ end
}
```

###### Concurrency is a breeze

This is one of the expected advantages of using functional programming techniques and thanks to Scalaz awesomeness the concurrent execution of examples is just one line of code!

```
fs.fragments.map(f => promise(executeFragment(arguments <| fs.arguments)(f))).sequence.get
```

##### A simple structure

This principle comes from the desire to unify the traditional ***specs*** approach of using blocks with `should` and `in` keywords with a more *literate* approach of having just free text.

In other words, there is no fundamental difference between an "Acceptance Testing" specification and a "Unit Test" specification. This is just a matter of the scale at which you're looking at things.

Moreover I found that having restrictions on the words I was supposed to use for my specification text didn't help me write the most appropriate descriptions of the system behavior or features.

The application of this principle is that a specification is composed of "Fragments" which can be some "Text" or some "Example" *simply appended together*. You can use whatever words you want to describe the examples `should`, `can`, `must`, whatever.

But wait! Nested structures serve 2 important purposes in ***specs***! They are used to control the scopes of variables that are applicable to examples and to compute the indentation when displaying the results.

How can this be done in ***specs2***?

###### Contexts

Setting up a proper context for an example, with "fresh" variables, which can be possibly inherited from a "parent" context, does not require any support from the library (difficult to get bugs with that, right :-) ?).

We simply use case class instances for each Example. Here is a demonstration:

    "When the user logs in"                      ^
      "his past history must be shown"           ! history().isShown^
      "if he selects tickets"                    ^
        "the list must be displayed"             ! tickets().list^
        "the total amount must be displayed"     ! tickets().total^
        "if he buys tickets"                     ^
          "his favorite payment type is shown"   ! buy().favorite

    trait Login {
      var loggedIn = false
      def login = loggedIn = true
      def logout = loggedIn = false
    }
    case class history() extends Login {
      login
      def isShown = loggedIn must beTrue
    }
    case class tickets() extends Login {
      login
      def list = pending
      def total = pending
    }
    case class buy() extends Login {
      val tickets = new tickets()
      def favorite = pending
    }


In the specification above, each example is using its own instance of a case class, having its own local variables which will never be overwritten by another example. Parent context is inherited by means of delegation. For example, in the "buy" context, there is an available `tickets` instance placing the system in the desired context.

There is a clear win here because the library doesn't have to propose new concepts, a new API to offer context management functionalites to: create contexts, share them, reuse them,...

Moreover in the "simple structure" above, there is no need for adding curly braces `{...}` to separate the specification elements. This makes the specification text remarkably close to what's going to be displayed when reported.

###### Indentation

In specs2, indentation is a feature but it doesn't have to be. For example you could just write the specification above as:

    "When the user logs in"                      ^
    "  his past history must be shown"           ! history().isShown^
    "  if he selects tickets"                    ^
    "    the list must be displayed"             ! tickets().list^
    "    the total amount must be displayed"     ! tickets().total^
    "    if he buys tickets"                     ^
    "      his favorite payment type is shown"   ! buy().favorite

Or you can leave ***specs2*** compute something reasonable for the indentation along the following rules:

  * when an example follows a text, it is indented
  * 2 successive examples will be at the same indentation level
  * when a text follows an example, this means that you want to describe a "subcontext", so the next examples will be indented with one more level

This strategy is most likely to bring appropriate results but there are additional formatting elements which can be inserted in order to adjust the indentation or just skip lines: `p, br, t, bt, end, endbr, endp`.

###### Operators

There are 2 major operators used by ***specs2*** when building a Specification: `^` and `!`.

`^` is used to "link" specification fragments together and `!` is used to declare the body of an example. The choice of those 2 symbols is mostly the result of the precedence rules in Scala. `+` binds more strongly than `!`, and `!` more strongly than `^`. This means that you don't need to add brackets to:

  * add strings with `+`: `"this is"+"my string" ^ "ok?"`
  * declare an example: `"this is some text" ^ "and this is an example description" ! success`

###### Forms

Having no explicit structure but just a "flow" of Fragments allows to insert other types of Fragments in a ***specs2*** specification.

The "description - expectation" format for specifying software is sometimes too verbose and tables are a much more effective way of packing up descriptions and expectations. This idea is  not new and a tool like [Fitnesse](http://fitnesse.org) has been offering this way of writing specifications for years now.

***specs2*** takes this idea further with 3 features:

 * the tables (called "Forms") are statically compiled so adding a new column in a decision table will not fail at runtime (and IDEs provide refactoring tools for better productivity)

 * the Forms are not limited to simple n x m grids and can be nested inside each other. This helps a lot in specifying domain objects where you have aggregates and lists of items

 * the Forms presentation and implementation can be encapsulated in the same class to be reused as a coherent block in other specifications

The main drawback (for now) of this approach is that it is not possible to see, in real-time, a modification done on a Form as can be seen in a browser with Fitnesse. There needs to be a compilation step, which in Scala, is not instantaneous.

##### But if you STILL want mutable specifications

There are at least 2 very good reason for that.

 1. you want a smooth migration path from ***specs*** to ***specs2*** because rewriting specifications from scratch, with a new syntax, does not bring a lot of value to your project

 2. chaining fragments hides the implementation of the examples and you have to navigate too much between the text and the example code to understand what's going on. This is especially true when writing _unit_ specifications where it's convenient to interleave short descriptions with blocks of code

Well, Scala is not a black-or-white language and mutation is definitely part of the toolbox. In the case of a specification DSL, we know the advantages: less syntax, and the drawbacks: uncontrolled side-effects.

Thus, in ***specs2*** it is possible to create specifications which look almost like the ones which can be created with ***specs***, with a bit less functionalities:

    import org.specs2.mutable._   // similar to the mutable package for Scala collections

    class MyMutableSpecification extends Specification {
      "This specification" should {
        "build examples with side-effects" in { success }
        "even use side-effects to avoid chaining expectations" in {
           1 must_== 2
           // the rest won't be executed
           success
        }
      }
    }

The important things to know are:

  * side-effects are only used to build the specification fragments, by mutating a variable
  * they are also used to short-circuit the execution of an example as soon as there is a failure (by throwing an exception)
  * if you build fragments in the body of examples or execute the same specification concurrently, the sky should fall down
  * "context" management is to be done with case classes or traits (see `org.specs2.examples.MutableSpec`)

##### Dependencies control

One classical impediment to software evolution is circular dependencies between packages in a project. The new ***specs2*** design makes sure that a layered architecture is maintained, from low-level packages to high-level ones:

  +    runner
  +    reporter
  +    specification mutable
  +    mock form
  +    matcher
  +    execute
  +               reflect  xml html  time json
  +    collection control  io  text  main data

In this scheme, a specification is no longer executable on its own, contrary to the ***specs*** design. It always need a runner.

Unfortunately this dependency specification is not yet enforced automatically in ***specs2*** test suite, but this kind of feature could be implemented in the future.

##### Implicit definitions control

There is a real tension to be solved here. On one hand, I want to encourage conciseness so that one should not have to stack too many traits on top of the Specification declaration to get the desired features. On the other hand, the more traits you add, the more implicits you bring in.

So the compromise is the following:

 + The `BaseSpecification` trait only allows to build Text fragments and Examples, without even any Matchers
 + On top of it, the `Specification` trait stacks lots of convenient functionalities to
     . use a concise notation for arguments
     . use matchers (with both `must` and `should`)
     . use predefined fragments and results (like `p`, `br`, `success`, `pending`,...)
     . and more
 + Specific traits are available to selectively deactivate features. For instance `NoAutoExamples` deactivates the creation of examples from simple expectations.

This way, if there is any conflict when inheriting from the `Specification` trait, it should be possible to either:

 + downgrade to the `BaseSpecification` and add the non-conflicting traits
 + mix-in specific traits to remove the problematic implicit definitions

##### Enter Scala 2.10

Scala 2.10 brings a new feature to the language, String interpolation. But the big difference with most languages is that it is possible to create your own interpolator! This removes one really annoying issue of specs2 < Scala 2.10: the omni-presence of `^` operators in acceptance specifications. Thanks to String interpolation the canonical "Hello World" example becomes:

    class HelloWorldSpec extends Specification { def is = s2$triple

      This is a specification to check the 'Hello world' string

      The 'Hello world' string should
        contain 11 characters              $e1
        start with 'Hello'                 $e2
        end with 'world'                   $e3
                                                          $triple

      def e1 = "Hello world" must have size(11)
      def e2 = "Hello world" must startWith("Hello")
      def e3 = "Hello world" must endWith("world")

    }


 - - -

<br/>

    """^
  include(xonly, chaining)                                                                                                 ^
  include(xonly, context)                                                                                                  ^
                                                                                                                           end


  val (e1, e2, e3) = ("$e1", "$e2", "$e3")

  val chaining = new Specification { def is =  args(color=false) ^
    "my example on strings" ! e1             // will fail
    def e1 = ("hello" must have size(1000) and startWith("hell")).isSuccess must beFalse
  }

  val context = new Specification { def is =
    "When the user logs in"                      ^
      "his past history must be shown"           ! history().isShown^
      "if he selects tickets"                    ^
        "the list must be displayed"             ! tickets().list^
        "the total amount must be displayed"     ! tickets().total^
        "if he buys tickets"                     ^
          "his favorite payment type is shown"   ! buy().favorite
  }
  trait Login {
    var loggedIn = false
    def login = loggedIn = true
    def logout = loggedIn = false
  }
  case class history() extends Login {
    login
    def isShown = loggedIn must beTrue
  }
  case class tickets() extends Login {
    login
    def list = success
    def total = success
  }
  case class buy() extends Login {
    val tickets = new tickets()
    def favorite = success
  }
}