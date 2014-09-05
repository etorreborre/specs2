package org.specs2
package guide
package matchers

object TraversableMatchers extends UserGuideCard {
  def title = "Traversable"
  def text = s2"""
Traversables can be checked with several matchers. If you want to check the size of a `Traversable`

 * check if it is empty
 ${snippet{Seq() must be empty}}
 ${snippet{Seq(1, 2, 3) must not be empty}}

 * check its size
 ${snippet{Seq(1, 2) must have size(2)}}
 ${snippet{Seq(1, 2) must have length(2)}} // equivalent to size
 ${snippet{Seq(1, 2) must have size(be_>=(1))}} // with a matcher

_note_: you might have to annotate the `haveSize` matcher when using some combinators. For example: `(futures: Future[Seq[Int]]) must haveSize[Seq[Int]](1).await`

 * check its ordering (works with any type `T` which has an `Ordering`)
 ${snippet{Seq(1, 2, 3) must beSorted}}

#### Check each element individually

Then you can check the elements which are contained in the Traversable

 * if a simple value is contained
 ${snippet{Seq(1, 2, 3) must contain(2)}}

 * if a value matching a specific matcher is contained
 ${snippet{Seq(1, 2, 3) must contain(be_>=(2))}}

 * if a value passing a function returning a `Result` is contained (`MatchResult`, ScalaCheck `Prop`,...)
 ${snippet{Seq(1, 2, 3) must contain((i: Int) => i must be_>=(2))}}

 * note that a `Seq[A]` is also a function `Int => A` so if you want to check that a sequence is contained in another you need to use a matcher
 ${snippet{Seq(Seq(1)) must contain(===(Seq(1)))}}

 * there are also 2 specialized matchers to check the string representation of the elements
 ${snippet{Seq(1234, 6237) must containMatch("23")     }}   `// matches with ".*23.*"`
 ${snippet{Seq(1234, 6234) must containPattern(".*234")}}   `// matches with !.*234"`

For each of the check above you can indicate how many times the check should be satisfied:

 * ${snippet{Seq(1, 2, 3) must contain(be_>(0)).forall}}  // this will stop after the first failure
 * ${snippet{Seq(1, 2, 3) must contain(be_>(0)).foreach}} // this will report all failures
 * ${snippet{Seq(1, 2, 3) must contain(be_>(0)).atLeastOnce}}
 * ${snippet{Seq(1, 2, 3) must contain(be_>(2)).atMostOnce}}
 * ${snippet{Seq(1, 2, 3) must contain(be_>(2)).exactly(1.times)}}
 * ${snippet{Seq(1, 2, 3) must contain(be_>(2)).exactly(1)}}
 * ${snippet{Seq(1, 2, 3) must contain(be_>(1)).between(1.times, 2.times)}}
 * ${snippet{Seq(1, 2, 3) must contain(be_>(1)).between(1, 2)}}

#### Check all elements

The other types of checks involve comparing the Traversable elements to other elements (values, matchers, function returning a `Result`)

 * with a set of values
 ${snippet{Seq(1, 2, 3, 4) must contain(2, 4)}}
 which is the same thing as
 ${snippet{Seq(1, 2, 3, 4) must contain(allOf(2, 4))}}

 * with a set of matchers
 ${snippet{Seq(1, 2, 3, 4) must contain(allOf(be_>(0), be_>(1)))}}

 * checking that the order is satisfied
 ${snippet{Seq(1, 2, 3, 4) must contain(allOf(be_>(0), be_>(1)).inOrder)}}

Note that `allOf` tries to make each check be successful at least once, even if it is on the same value. On the other hand, if you want to specify that each check must succeed on a *different* value you should use `onDistinctValues`. For example this will fail:
${snippet{Seq(1) must contain(allOf(1, 1)).onDistinctValues}}

The `eachOf` method does the same thing (and this example will fail as well):
${snippet{Seq(1) must contain(eachOf(1, 1))}}

Another frequent use of Traversable matchers is to check if the Traversable have the right number of elements. For this you can use:

 * `atLeast`, which is actually another name for `allOf`, where the traversable can contain more elements than required
 ${snippet{Seq(1, 2, 3, 4) must contain(atLeast(2, 4))}}

 * `atMost` where the traversable can not contain more elements than required
 ${snippet{Seq(2, 3) must contain(atMost(2, 3, 4))}}

 * `exactly` where the traversable must contain exactly the specified number of elements
 ${snippet{Seq(1, 2) must contain(exactly(2, 1))}}

The `atLeast/atMost/exactly` operators work on distinct values by default (because this is easier for counting the correspondence between actual values and expected ones). However you can use `onDistinctValues(false)` if you don't care.

Finally, if you want to get the differences between 2 traversables:

 ${snippet{Seq(2, 4, 1) must containTheSameElementsAs(Seq(1, 4, 2))}}


"""
}
