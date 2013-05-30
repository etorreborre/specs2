package org.specs2
package specification

import text._
import execute.Skipped
import matcher._

class FragmentsBuilderSpec extends script.Specification with ResultMatchers with Groups with GivenWhenThen {  def is = s2"""

 In a Specification, the `contents` variable stores an instance of the Fragments class,
 which is merely a list of fragments. Those fragments are:

  * `SpecStart` / `SpecEnd` elements. A `SpecStart` contains the arguments applicable to all the specification
    fragments, and an identifier for the Specification:`SpecName`. The corresponding `SpecEnd` fragment must have the
    same name

  * `Text` elements which are simply embedding a String

  * `Example` elements. An Example has:
     * a description which can contain some markup tags to render it as code for example
     * an executable block returning a `Result`: a `Success`, a `Failure`

  * `Step` fragments which are not reported but execute an action

SpecStart/SpecEnd
=================

 In a specification SpecStart and end fragments are automatically added
   + SpecStart is always the first fragment and SpecEnd the last
   + the SpecStart object contains the specification arguments
   + SpecStart and SpecEnd have the same name
   + The SpecStart element can be created by adding a title to the specification
   + When a title is created there is only one SpecStart in the specification
   + A title can be added before arguments are declared
   + A title can be added after arguments are declared
   + A title can not be empty

Arguments
=========

   Arguments can be added in different place in the spec
     + new Arguments values are added to the existing ones
     + and override them if already declared
     + it also works with the map method in BaseSpecification

Links
=====

   A specification can be linked
     + and included
     + or just referenced
     + and hidden, it'll be executed but not reported

   + Several specifications can be linked at once
   + A specification can reference itself without creating a loop


How to create an Example
========================

 + An example is simply created with `string ! e1` where e1 returns a `Result`
 + An example can have its description marked as `code` for nice html rendering
 + An example can use its own description
 + An example can use a partial function to extract values from its text
   + the description must be stripped out of value markers

 + An Error in the Example body will not fail the example creation
   + even if it is an AssertionError

 An example has a `matches` method to match its description against a regexp
   + it returns true if there is a match
   + it works even if there are newlines in the description


Other elements
==============

 + A Fragments object can be created by appending fragment objects
 + Or simply by casting a String to a Fragments object

 A Step can be created from an Either[Result, T]
   + from a Left(Skipped())
   + from a Right(value)

 A Step can be created with a stopOnFail value
   + with a stopOnFail = true
   + with a stopOnFail value throwing an exception

 A Step can be created with an action
   + executing ok
   + throwing an exception

                                                                                                           """

  "start and end" - new group with specifications {
    eg  := (fragments.head must haveClass[SpecStart]) and (fragments.last must haveClass[SpecEnd])
    eg  := content.specStart.arguments.xonly
    eg  := content.specStart.specName must beTheSameAs(content.specEnd.specName)
    eg  := content.specStart.title must_== "title"
    eg  := content.fragments.map(_.toString) must contain(equalTo("SpecStart(title)")).exactly(1.times)
    eg  := content.specStart.arguments.xonly must beTrue
    eg  := (content2.specStart.title must_== "title") and (content2.specStart.arguments.xonly must beTrue)
    eg  :=  content6.specStart.title must not(beEmpty)
  }
  "arguments" - new group with specifications {
    eg := (content3.specStart.arguments.xonly must beTrue) and (content3.specStart.arguments.include must_== "t1")
    
    eg := content4.specStart.arguments.include must_== "t2"
    eg := content5.specStart.arguments.sequential must beTrue
  }
  "links" - new group with specifications {

    eg := parentSpec1.content.fragments.toList must
              beLike { case SpecStart(_,_,_) :: Text(_) :: SpecStart(_,_,Linked(Some(l), false, false)) :: rest => ok }
    eg := parentSpec2.content.fragments.toList must
              beLike { case SpecStart(_,_,_) :: Text(_) :: SpecStart(_,_,Linked(Some(l), true, false)) :: rest => ok }
    eg := parentSpec3.content.fragments.toList must
              beLike { case SpecStart(_,_,_) :: Text(_) :: SpecStart(_,_,Linked(Some(l), false, true)) :: rest => ok }

    eg := parentSpec4.content.fragments.toList must
              beLike { case SpecStart(_,_,_) :: Text(_) ::
                         SpecStart(_,_,Linked(Some(_), false, false)) :: Text(_) :: SpecEnd(_,_) ::
                         SpecStart(_,_,Linked(Some(_), false, false)) :: Text(_) :: SpecEnd(_,_) :: rest => ok }

    eg := selfReferencing.content must terminate(retries = 3, sleep = 100.millis)
  }
  "examples" - new group {
    eg := success

    eg := Example(FormattedString.code("a == b"), success).desc.toXml.toString must startWith("<code")

    eg := ("description" ! ((s: String) => s must_== "description")).body() must beSuccessful

    val soExample = "given the name: ${eric}, then the age is ${18}" ! so {
      case (name: String, age: String) => age.toInt must_== 18
    }
    eg := soExample.body() must beSuccessful
    eg := soExample.desc.toString must_== "given the name: eric, then the age is 18"

    def execute = FragmentExecution.executeFragment(args())
    eg := execute("example" ! { throw new NoSuchMethodError("flushBuffer"); success }).toString must beMatching(".*ThrowableException.*NoSuchMethodError\\: flushBuffer.*")
    eg := execute("example" ! { throw new AssertionError(); success }).toString must not contain("Fragment evaluation error")

    eg := ("Eric" ! success).matches("E.*")
    eg := ("Eric\nT." ! success).matches("E.*T.*")
  }
  "other elements" - new group {
    eg := ("t" ^ "t2").middle must have size(2)
    eg := ("t":Fragments).middle must have size(1)

    eg := Step.fromEither(Left(Skipped())).execute must beSkipped
    eg := Step.fromEither(Right("value")).execute must beSuccessful

    eg := Step(stopOnFail = true).execute must beSuccessful
    eg := Step(stopOnFail = {throw new Exception; true}).execute must beError

    eg := Step(1).execute must beSuccessful
    eg := Step({throw new Exception; 1}).execute must beError

  }

  trait specifications extends TerminationMatchers {
    lazy val spec1 = new Specification { def is = "title".title ^ xonly ^ "text" }
    lazy val spec2 = new Specification { def is = xonly ^ "title".title ^ "text" }
    lazy val content = spec1.content
    lazy val content2 = spec2.content
    lazy val content3 = new Specification { def is = xonly ^ args(include="t1") ^ "title".title ^ "text" }.content
    lazy val content4 = new Specification { def is = args(include="t1") ^ "title".title ^ args(include="t2") ^ "text" }.content
    lazy val parentSpec1 = new Specification { def is = "e1" ^ link(spec1) }
    lazy val parentSpec2 = new Specification { def is = "e1" ^ see(spec2) }
    lazy val parentSpec3 = new Specification { def is = "e1" ^ link(spec1.hide) }
    lazy val parentSpec4 = new Specification { def is = "e1" ^ link(spec1, spec2) }
    lazy val selfReferencing: Specification = new Specification { def is = "e1" ^ see(selfReferencing) }

    trait CustomSpecification extends Specification {
      override def map(fs: =>Fragments) = "title".title ^ fs ^ "end of the spec"
    }
    lazy val content5 = new CustomSpecification { def is = sequential ^ "text" }.content
    lazy val content6 = new Specification with GivenWhenThen {
      val number: Given[Int] = (_:String).toInt
      def is = "a number ${0}" ^ number
    }.content

    def fragments = content.fragments
  }
}