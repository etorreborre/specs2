package org.specs2
package guide
package structure

object Unit extends UserGuideVariables {
  def section = s"""
### Unit specifications

#### Methods

Those are all the methods which you can use to create fragments in a unit specification:

 * `can`: create a group of Examples, with the preceding Text fragment appended with `can`

```
"a configuration" can {
  "have a name" in { ... }
}
```

 * &gt;&gt;: create an Example or a group of Examples (with no appended text)

```
"a configuration may" >> {
  "have a name" in { ... }
}
```

Note that you can use a `for` loop to create examples with the `examplesBlock` method:

```
"this system has 5 examples" >> {
  examplesBlock { (1 to 5) foreach { i => "example "+i >> ok } }
}
```
And you can also use a `for` loop with the `Result.unit` method to create a block of expectations:

```
"this example has 5 expectations" in {
  Result.unit { (1 to 5) foreach { i => i must_== i } }
}
```

 * `title`: give a title to the Specification

```
"My spec title".title
// file path can be used to specify a different path for the html reporting
"My spec title".title(filePath = "com/MySpec.html")
```
 * `args`: create arguments for the specification

 * `.txt` or `textFragment`: create a `Text` fragment

```
"this is a text fragment".txt

textFragment("this is a text fragment")
```
 * `step`: create a `Step`

        step { initializeDatabase() }

 * `action`: create an `Action`

        action { justDoIt }

 * `link`: create a link to another specification

        link("how" ~ ("to do hello world", new HelloWorldSpec))

 * `see`: add a link to another specification without including its fragments for execution

        see(new HelloWorldSpec)

 * `include` to include another specification

        include(new HelloWorldSpec)

 * `p, br, t, bt, end, endp`: add a formatting fragment

#### Full example

To make things more concrete here is a full example:

    import mutable._
    import specification._
    import execute.Success

    /**
     * This specification shows how to use the mutable.Specification trait to create a unit Specification
     * where the fragments are built using a mutable variable
     */
    class MutableSpec extends Specification {

      // A title can be added at the beginning of the specification
      "MutableSpec".title
      // arguments are simply declared at the beginning of the specification if needed
      args(xonly=true)

      "This is a unit specification showing the use of different methods".txt

      // a step to execute before the specification must be declared first
      step {
        // setup your data or initialize your database here
        success
      }

      "'Hello world'" should {
        "contain 11 characters" in {
          "Hello world" must have size(11)
        }
        "start with 'Hello'" in {
          "Hello world" must startWith("Hello")
        }
        /**
         * a failing example will stop right away, without having to "chain" expectations
         */
        "with 'world'" in {
          // Expectations are throwing exception by default so uncommenting this line will
          // stop the execution right away with a Failure
          // "Hello world" must startWith("Hi")

          "Hello world" must endWith("world")
        }
      }
      /**
       * "Context management" is handled through the use of traits or case classes
       */
      "'Hey you'" should {
        // this one uses a "before" method
        "contain 7 characters" in context {
          "Hey you" must have size(7)
        }
        // System is a Success result. If the expectations fail when building the object, the example will fail
        "contain 7 characters" in new system {
          string must have size(7)
        }
        // otherwise a case class can be used but the example body will be further down the file
        "contain 7 characters" in system2().e1
      }
      // you can add links to other specifications with `link`
      // they will be executed when this one is executed. If you don't want this to happen
      // you can use `see` instead of `link`
      link("how" ~ ("to do hello world", new HelloWorldSpec))
      // you can include other specifications with `include`
      include(new HelloWorldSpec)

      // a step to execute after the specification must be declared at the end
      step {
        // close the database here
        success
      }


      object context extends Before {
        def before = () // do something to setup the context
      }
      // we need to extend Scope to be used as an Example body
      trait system extends Scope {
        val string = "Hey you"
      }
      case class system2() {
        val string = "Hey you"
        def e1 = string must have size(7)
      }
    }
  """
}
