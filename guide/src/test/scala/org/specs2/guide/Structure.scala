package org.specs2
package guide

object Structure extends UserGuidePage { def is = s2"""

### Styles

In a Specification you generally want to include 2 things:

 - some informal text describing what the system/application/function should do
 - some Scala code specifying exactly inputs and expected outputs

<p/>
With $specs2 you have 2 main ways to do this:

 - you can create an *"Acceptance"* specification where all the informal text is written in one place and the code is written somewhere else. The name "acceptance" comes from the fact that it might be easier for a non-developer to just read some text to validate your specification

 - you can create a *"Unit"* specification where the code is interleaved with the text. The name "unit" comes from the fact that Unit specifications have a structure which is close to unit tests in "classical" frameworks such as JUnit

Both ways of writing specifications have advantages and drawbacks:

 - Acceptance specifications are easier to read as a narrative but require navigation between the text and the code. You also need to define an `is` method holding the body of the specification

 - Unit specifications are easier to navigate but the text tends to be lost in a sea of code

### Acceptance specification

An acceptance specification extends `org.specs2.Specification` and defines the `is` method. You can implement this method with an interpolated **`s2`** string: ${snippet {
class MySpecification extends org.specs2.Specification { def is = s2"""

 this is my specification
   where example 1 must be true           $e1
   where example 2 must be true           $e2
                                          """

  def e1 = 1 must_== 1
  def e2 = 2 must_== 2
}
}}

The `s2` string contains the text of your specification as well as some references to methods (`e1` and `e2`) defining *`Results`*. When the Specification is executed, the `s2` string is analysed and 2 `Examples` are created then executed:

 - one `Example` with the description "where example 1 must be true" and the code `1 must_== 1`
 - another `Example` with the description "where example 2 must be true" and the code `2 must_== 2`

<p/>
All the rest, `"this is my specification"`, is parsed as `Text` and is not executed.

### Unit specification

A unit specification extends `org.specs2.mutable.Specification` and uses the `>>` operator to create "blocks" containing `Texts` and `Examples`: ${snippet {
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

 - there is no need to define an `is` method (this means that a mutable variable is used to collect the `Texts` and `Examples` hence the `mutable` package name)
 - the code is close to each piece of text it specifies

<p/>
However once a specification is created with all its `Texts` and `Examples`, the execution will be the same, whether it is an Acceptance one or a Unit one.

The `>>` blocks can be nested and this allows you to structure your specification so that the outermost blocks describe a general context while the innermost ones describe more specific contexts. A similar effect can be achieved by simply indenting text in an acceptance specification.

### Expectations

There is another major difference between the acceptance specifications and unit specifications. The first style encourages you to write [one expectation per example](http://bit.ly/one_assertion_per_test) while the second allows to use several. One expectation per example is useful because when a specification fails, you know immediately what is wrong. However it is sometimes expensive to setup data for an example. In that case, having several expectations sharing the same setup might be preferable.

The good news is that for each of the 2 main styles, acceptance and unit, you can choose exactly which "Expectation mode" you prefer if the default mode is not convenient.

#### Functional expectations

In an acceptance specification, by default, the `Result` of an `Example` is always given by the last statement of its body. For instance, this example will never fail because the first expectation is "lost":${snippet{
// this will never fail!
s2"""
  my example on strings $e1
"""
  def e1 = {
    // because this expectation will not be returned,...
    "hello" must have size (10000)
    "hello" must startWith("hell")
  }
}}

If you want to get both expectations you will need to use `and` between them: ${snippet{
s2"""
  my example on strings $e1
"""
  def e1 = ("hello" must have size (10000)) and
           ("hello" must startWith("hell"))
}}

This is a bit tedious and not very pleasing to read so you can see why this mode encourages one expectation per example only! If you want to declare several expectations per example, you can mix-in the `org.specs2.execute.ThrownExpectations` trait to the specification.

#### Thrown expectations

With a unit specification you get "thrown expectations" by default. When an expectation fails, it throws an exception and the rest of the example is not executed: ${snippet {
class MySpecification extends org.specs2.mutable.Specification {
  "This is my example" >> {
    1 must_== 2 // this fails
    1 must_== 1 // this is not executed
  }
}
}}

It is also possible to use the "functional" expectation mode with a unit specification by mixing in the `org.specs2.matcher.NoThrownExpectations` trait.

$NowLearnTo
 - use ${"matchers" ~/ Matchers} to specify the body of your examples
 - set up ${"contexts" ~/ Contexts} for the examples
 - control the ${"execution" ~/ Execution} of a specification
 - ${"run" ~/ Runners} a specification

$vid

$AndIfYouWantToKnowMore

 - ${"skip" ~/ SkipExamples} examples
 - use a ${("different syntax" ~ MutableSpecSyntax).mute} for mutable specifications
 - collect ${"*all* expectations" ~/ GetAllExpectations}
 - mark examples as ${"pending until they are fixed" ~/ PendingUntilFixedExamples}
 - add ${"references to other specifications" ~/ ReferenceOtherSpecifications}
 - create example descriptions spanning ${"several lines" ~/ MultilineDescriptions}
 - add ${"new lines and tabs" ~/ SpecificationFormatting} in unit specifications
 - create a trait which will ${"decorate or transform other specifications" ~/ SpecificationTemplate} when mixed-in
 - create ${"*auto-examples*" ~/ AutoExamples} where the code *is* the description of the `Example`
 - integrate ${"snippets of code" ~/ CaptureSnippets} to your specification
 - use ${"named examples" ~/ NamedExamples} in acceptance specifications to get default example names
 - use ${"scripts and auto-numbered examples" ~/ AutoNumberedExamples} to completely separate the specification text from the code
 - use ${"lightweight specifications" ~/ LightweightSpecs} to reduce the number of implicits in scope
 - use the ${"Given/When/Then style" ~/ GivenWhenThenStyle} to structure specifications
 - use ${"Forms" ~/ UseForms} to create executable HTML tables in your specification
 - use the ${"command line arguments" ~/ UseCommandLineArguments} to define the body of an example
 - add ${"debug statements" ~/ DebugStatements}
 - print ${"execution data" ~/ PrintExecutionData}
 - extend the specification by creating examples ${"_while it is executing_" ~/ CreateOnlineSpecifications}

$vid

"""
}
