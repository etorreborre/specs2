package org.specs2
package guide

object LightweightSpecs extends UserGuidePage { def is = s2"""

The default `Specification` class mixes in most of $specs2 features in order to make it very easy to write your first specifications without worrying too much about where the feature resides.

There at least 2 downsides to this approach:

  * inside a `Specification` you get lots of inherited method names, encumbering the namespace
  * many of the traits mixed-in the `Specification` class define implicit methods and those methods have a big impact on compilation times

It is thus possible to use another class, `org.specs2.Spec` (or `org.specs2.mutable.Spec`), which only provides the minimum number of implicits to create specifications.

With the `Spec` class you can create examples and expectations with simple matchers. For example:${snippet{
class HelloWorldSpec extends Spec { def is = s2"""

 This is a specification to check the 'Hello world' string

 The 'Hello world' string should
   contain 11 characters                                         $e1
   start with 'Hello'                                            $e2
   end with 'world'                                              $e3
                                                                 """

 def e1 = "Hello world" must haveSize(11)
 def e2 = "Hello world" must startWith("Hello")
 def e3 = "Hello world" must endWith("world")
}
}}

Or, for mutable specs:${snippet{
class HelloWorldSpec extends mutable.Spec {

  addParagraph("This is a specification to check the 'Hello world' string")

 "The 'Hello world' string" should {
   "contain 11 characters" in {
     "Hello world" must haveSize(11)
   }
   "start with 'Hello'" in {
     "Hello world" must startWith("Hello")
   }
   "end with 'world'" in {
     "Hello world" must endWith("world")
   }
 }

}
}}

If you compare those 2 specifications with the "HelloWorldSpec" examples using `Specification` you will notice some differences:

 - you cannot write `"Hello world" must have size(11)`
 - you cannot write `"This is a specification to check the 'Hello world' string".txt` to add some text to a mutable spec

But not all is lost! For each functionality you might want to use there is a trait which you can mix-in to get it.

### Adding features

When creating expectations:

 Feature                                          | Trait                                           | Comment
 ------------------------------------------------ | ----------------------------------------------- | ----------------------
 Use `should` for expectations                    | `org.specs2.matcher.ShouldMatchers`             |
 Describe expectations with `==>`                 | `org.specs2.matcher.ExpectationsDescription`    |
 Describe expectations with `aka` and `must`      | `org.specs2.matcher.MustExpectations`           |
 Use `list must have size(3)`                     | `org.specs2.matcher.TraversableMatchers`        | For each matcher you want to use after `be/have/not`, you need to mix-in the corresponding matchers trait
 Use matchers in `contain` or `beSome` matchers   | `org.specs2.matcher.ValueChecks`                |
 Use `===`, `====` to check for equality          | `org.specs2.matcher.TypedEqual`                 |
 Create matchers from functions                   | `org.specs2.matcher.MatchersCreation`           |
 Combine `MatchResults` with `and/or`             | `org.specs2.matcher.MatchResultsCombinators`    |
 Transform a `Matcher[T]` to `Matcher[Seq[T]]`    | `org.specs2.matcher.SequenceMatchersCreation`   |
 Use `forall(values)(t => t must xxx)`            | `org.specs2.matcher.SequenceMatchersCreation`   | You can combine all matchers features by using the `org.specs2.matcher.MatchersImplicits` trait
 Set failed expectations as Pending               | `org.specs2.execute.PendingUntilFixed`          |

When creating acceptance specifications

 Feature                                                      | Trait                                                  | Comment
 ------------------------------------------------------------ | ------------------------------------------------------ | ----------------------
 Interpolate anything else than a `Result` in a `s2` string   | `org.specs2.specification.create.S2StringContext`      |
 Use "bang" examples: `"example" ! ok`                        | `org.specs2.specification.dsl.ExampleDsl`              |
 Create and append `Fragments` with `^`                       | `org.specs2.specification.dsl.FragmentsDsl`            |
 Add arguments and a title to `Fragments` with `^`            | `org.specs2.specification.dsl.SpecStructureDsl`        |
 Create a title with `"A title".title"`                       | `org.specs2.specification.dsl.TitleDsl`                |
 Create references to other specifications                    | `org.specs2.specification.dsl.ReferenceDsl`                 |
 Create steps and actions                                     | `org.specs2.specification.dsl.ActionDsl`               |
 Use tags                                                     | `org.specs2.specification.dsl.TagDsl`                  | To use all of the `Dsl` traits use `AcceptanceDsl`

When creating mutable specifications

 Feature                                                      | Trait                                                  | Comment
 ------------------------------------------------------------ | ------------------------------------------------------ | ----------------------
 Use "bang" examples: `"example" ! ok`                        | `org.specs2.specification.dsl.mutable.ExampleDsl`      |
 Create a title with `"A title".title"`                       | `org.specs2.specification.dsl.mutable.TitleDsl`        |
 Set arguments                                                | `org.specs2.specification.dsl.mutable.ArgumentsDsl`    |
 Create references to other specifications                    | `org.specs2.specification.dsl.mutable.ReferenceDsl`         |
 Create steps and actions                                     | `org.specs2.specification.dsl.mutable.ActionDsl`       |
 Add text and paragraphs                                      | `org.specs2.specification.dsl.mutable.TextDsl`         |
 Use tags                                                     | `org.specs2.specification.dsl.mutable.TagDsl`          | To use all of the `Dsl` traits use `MutableDsl`
"""
}
