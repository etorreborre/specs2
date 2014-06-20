package org.specs2
package guide

object Structure extends UserGuidePage { def is = s2"""

### Styles

In a Specification you generally want to include 2 things:

 - some informal text describing what the system/application/function should do
 - some code specifying exactly what is expected when something is executed

With ***specs2*** you have 2 main ways to do this:

 - you can create an "Acceptance" specification where all the informal text is written in one place and the code is written elsewhere. The name "acceptance" comes from the fact that it might be easier for a non-developer to read to text to validate your specification

 - you can create a "Unit" specification where the code is interleaved with the text. The name "unit" comes from the fact that Unit specifications have a structure which is close to unit tests in "classical" frameworks such as JUnit

Both ways of writing specifications have advantages and drawbacks:

 - Acceptance specifications are easier to read as a narrative but require navigation between the text and the code. You also need to define a `is` method holding the body of the specification.
 - Unit specifications are easier to navigate but the text tends to be lost in a sea of code

#### Acceptance specification

An acceptance specification extends `org.specs2.Specification` and defines the `is` method. You can implement this method with an interpolated **`s2`** string: ${snippet{
class MySpecification extends org.specs2.Specification { def is = s2"""

 this is my specification
   where example 1 must be true           $e1
   where example 2 must be true           $e2
                                          """

  def e1 = 1 must_== 1
  def e2 = 2 must_== 2
}
}}

The `s2` string contains the text of your specification as well as some references to methods (`e1` and `e2`) defining *`Results`*. When the Specification is executed the `s2` string is analysed and 2 `Examples` are created then executed:

 - one `Example` with the description "where example 1 must be true" and the code `1 must_== 1`
 - another `Example` with the description "where example 2 must be true" and the code `2 must_== 2`

All the rest, `"this is my specification"`, is parsed as `Text` and is not executed.

#### Unit specification

A unit specification extends `org.specs2.mutable.Specification` and uses the `>>` operator to create "blocks" containing `Texts` and `Examples`: ${snippet{
class MySpecification extends org.specs2.mutable.Specification {
  "this is my specification" >> {
    "where example 1 must be true" >> {
      1 must_== 1
    }
    "where example 2 must be true" >> {
      2 must_== 2
    }
  }
}
}}

This specification creates one piece of `Text` and 2 `Examples` as before but:

 - there is no need to define an `is` method (this means that a mutable variable is used to collect the `Texts` and `Examples` hence the `mutable` package)
 - the code is close to each piece of text it specifies

However once a specification is created with all its `Texts` and `Examples`, the execution will be the same, whether it is an Acceptance one or a Unit one.

The `>>` blocks can be nested and this allows you to structure your specification so that the outermost blocks describe the general context while the innermost ones describe a more specific context. A similar effect can be achieved by simply indenting text in an acceptance specification.

### Expectations

There is another major difference between the acceptance specifications and unit specifications. The first style encourages you to write [one expectation per example](http://bit.ly/one_assertion_per_test) while the second allows to use several. One expectation per example is useful because when a specification fails, you know immediately what is wrong. However it is sometimes expensive to setup data for an example so having several expectations sharing the same setup is sometimes what you want.

The good news is that for each of the 2 main styles, acceptance and unit, you can choose exactly which mode you prefer if the default mode is not convenient.

#### Functional expectations

In an acceptance specification, by default, the `Result` of an `Example` is always given by the last statement of its body. For instance, this example will never fail because the first expectation is "lost": ${snippet{
// this will never fail!
s2"""
  my example on strings $e1
"""
  def e1 = {
    // because this expectation will not be returned,...
    "hello" must have size(10000)
    "hello" must startWith("hell")
  }
}}

If you want to get both expectations you will need to use `and` between them: ${snippet{
s2"""
  my example on strings $e1
"""
  def e1 = ("hello" must have size(10000)) and
           ("hello" must startWith("hell"))
}}

This is a bit tedious and not very pleasing to read so you can see why this mode encourages one expectation per example only. If you need several expectations per example, you can need to mix-in the `org.specs2.execute.ThrownExpectations` trait to the specification which is the one used for unit specifications by default.

##### Thrown expectations

With a unit specification you get "thrown expectations". When an expectation fails, it throws an exception and the rest of the example is not executed: ${snippet{
class MySpecification extends org.specs2.mutable.Specification {
  "This is my example" >> {
    1 must_== 2  // this fails
    1 must_== 1  // this is not executed
  }
}
}}

It is also possible to use the "functional" expectation mode with a unit specification by mixing in the `org.specs2.execute.NoThrownExpectations` trait.

### Now learn how to...

 - use ${"matchers" ~/ Matchers} to specify the body of your examples
 - set up ${"contexts" ~/ Contexts} for the examples
 - control the ${"execution" ~/ Execution} of a specification
 - ${"run" ~/ Runners} a specification

### And if you want to know more

 - create ${"*auto-examples*" ~ AutoExamples} where the code *is* the description of the `Example`
 - integrate ${"snippets of code" ~ CaptureSnippets} to your specification
 - ${"skip" ~ SkipExamples} examples
 - collect ${"*all* expectations" ~ GetAllExpectations}
 - use scripts and auto-numbered examples
 - use the Given-When-Then style for structuring specifications
 - mark examples as pending until they succeed
 - use the command line arguments to define the body of an example
 - create a trait which will add specification fragments before or after an other specification when mixed-in

"""
/*
##### All

The `org.specs2.specification.AllExpectations` trait goes further and gives you the possibility to report all the failures of an Example without stopping at the first one. This enables a type of specification where it is possible to define lots of expectations inside the body of an example and get a maximum of information on what fails and what passes: ${snippet{

  import org.specs2.specification.AllExpectations
  import org.specs2.mutable.Specification

  class AllExpectationsSpec extends Specification with AllExpectations {
    "In this example all the expectations are evaluated" >> {
      1 === 2  // this fails
      1 === 3  // this also fails
      1 === 1
    }
    "There is no collision with this example" >> {
      10 === 11 // this fails
      12 === 12
      13 === 31 // this also fails
    }
  }
}}

The second example above hints at a restriction for this kind of Specification. The failures are accumulated for each example by mutating a shared variable. "Mutable" means that the concurrent execution of examples will be an issue if done blindly. To avoid this, the `AllExpectations` trait overrides the Specification arguments so that the Specification becomes [isolated](#Isolated+variables) unless it is already `isolated` or `sequential`.


#### Auto-Examples

If your specification is about showing the use of a DSL or of an API, you can elide a description for the Example. This functionality is used in ***specs2*** to specify matchers: ${snippet{
  s2"""
beNone checks if an element is None
${ None must beNone }
${ Some(1) must not be none }
"""
}}

In that case, the text of the example will be extracted from the source file and the output will be:
```
beNone checks if an element is None
+ None must beNone
+ Some(1) must not be none
```
#### G / W /T

The Given/When/Then style for writing specifications is described ${new GivenWhenThenPage}.

#### DataTables

[DataTables](org.specs2.guide.Matchers.html#DataTables) are generally used to pack lots of expectations inside one example. A DataTable which is used as a `Result` in the body of an Example will only be displayed when failing. If, on the other hand you want to display the table even when successful, to document your examples, you can omit the example description and inline the DataTable directly in the specification: ${snippet{

  class DataTableSpec extends Specification with Tables { def is =

    "adding integers should just work in scala"  ^ eg {
      "a"   | "b" | "c" |
        2    !  2  !  4  |
        1    !  1  !  2  |>
        { (a, b, c) =>  a + b must_== c }
    }
  }
}}
This specification will be rendered as:
```adding integers should just work in scala
+  a | b | c |
   2 | 2 | 4 |
   1 | 1 | 2 |
```
#### Example groups

When you create acceptance specifications, you have to find names to reference your examples, which can sometimes be a bit tedious. You can then get some support from the `${fullName[specification.Grouped]}` trait. This trait provides group traits, named `g1` to `g22` to define groups of examples. Each group trait defines 22 variables named `e1` to `e22`, to define examples bodies. The specification below shows how to use the `Grouped` trait: ${snippet{

  class MySpecification extends Specification with Examples { def is =  s2"""
  first example in first group                                        ${g1.e1}
  second example in first group                                       ${g1.e2}

  first example in second group                                       ${g2.e1}
  second example in second group                                      ${g2.e2}
  third example in second group, not yet implemented                  ${g2.e3}
  """
  }

  trait Examples extends specification.Grouped with matcher.Matchers {
    // group of examples with no description
    new g1 {
      e1 := ok
      e2 := ok
    }
    // group of examples with a description for the group
    "second group of examples" - new g2 {
      e1 := ok
      e2 := ok
    }
  }
}}

Note that, if you use groups, you can use the example names right away, like `g2.e3`, without providing an implementation, the example will be marked as `Pending`.

##### Isolation

You can define additional variables in your group traits: ${snippet{
  // 8<--
  class MySpecification extends Groups { def is = ""
    // 8<--
    trait Local {
      def service: Service = new LocalService
    }
    "a group of examples" - new g1 with Local {
      e1 := ok
      e2 := ok
    }
    "another group of examples" - new g2 with Local {
      e1 := ok
      e2 := ok
    }
    // 8<--
  }
}}

However, the `service` variable will be shared by all the examples of each group, which can be potentially troublesome if that variable is mutated. If you want to provide complete isolation for each example, you should instead use the `${fullName[specification.Groups]}` trait and call each group as a function: ${snippet{

  class MySpecification extends Specification with Examples { def is = s2"""

  first example in first group                     ${g1().e1}
  second example in first group                    ${g1().e2}
                                                   """
  }

  trait Examples extends Groups with matcher.Matchers {
    trait Local {
      def service: Service = new LocalService
    }
    "a group of examples" - new g1 with Local {
      // each example will have its own instance of Service
      e1 := ok
      e2 := ok
    }
  }
}}

#### Tags

Tags can be used for 2 purposes: example selection and formatting.

##### Selection

You can tag specific examples or entire sections of a specification and execute them selectively from the command line. See [HowTo](org.specs2.guide.HowTo.html#Tags) on how to use tags.

##### Formatting

You can use the `formatSection` or `formatTag` methods to specify the formatting of `Texts` and `Examples` fragments with the following parameters:

 - `flow`: the fragment (`Text` or `Example`) shouldn't be reported with automatic indenting (default = `false`, set automatically to `true` when using `s2` interpolated strings)
 - `markdown`: the fragment is using Markdown (default = `true`)
 - `verbatim`: indented text with more than 4 spaces must be rendered as a code block (default = `true`)


"""
  trait Service
  class LocalService extends Service
  */
}

/*

class Structure2 extends UserGuidePage { def is = s2"""

### Presentation

In this section you will learn how to:

 * create examples and expectations
 * define contexts to execute actions before/after examples
 * structure a group of specifications by using links
 * specify the execution strategy
 * work with Unit specifications

 $${Examples.section  }
 $${Contexts.section  }
 $${Links.section     }
 $${Execution.section }
 $${Unit.section      }
  ----
  """
}
*/