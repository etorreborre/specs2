package org.specs2
package specification

import text._
import execute.Skipped
import matcher._
import Fragments._
import control.Functions._

class FragmentsBuilderSpec extends script.Specification with ResultMatchers with Groups {  def is = s2"""

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

How to create an Example
========================

 + An example is simply created with `string ! e1` where e1 returns a `Result`
 + An example can have its description marked as `code` for nice html rendering
 + An example can use its own description
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
    eg  := content.specStart.title must_== "title1"
    eg  := content.fragments.map(_.toString) must contain(equalTo("SpecStart(title1)")).exactly(1.times)
    eg  := content.specStart.arguments.xonly must beTrue
    eg  := (content2.specStart.title must_== "title2") and (content2.specStart.arguments.xonly must beTrue)
  }
  "arguments" - new group with specifications {
    eg := (content3.specStart.arguments.xonly must beTrue) and (content3.specStart.arguments.include must_== "t1")
    
    eg := content4.specStart.arguments.include must_== "t2"
    eg := content5.specStart.arguments.sequential must beTrue
  }
  "links" - new group with specifications {

    eg := startTextEndFragments(parentSpec1.content) must
              beLike { case (s: SpecStart) :: (t: Text) :: SpecStart(_,_,Linked(Some(l), false, false),_) :: rest => ok }
    eg := startTextEndFragments(parentSpec2.content) must
              beLike { case (s: SpecStart) :: (t: Text) :: SpecStart(_,_,Linked(Some(l), true, false),_) :: rest => ok }
    eg := startTextEndFragments(parentSpec3.content) must
              beLike { case (s: SpecStart) :: (t: Text) :: SpecStart(_,_,Linked(Some(l), false, true),_) :: rest => ok }

    eg := startTextEndFragments(parentSpec4.content) must
              beLike { case (s: SpecStart) :: (t1: Text) ::
                         SpecStart(_,_,Linked(Some(_), false, false), _) :: (t2: Text) :: (e1: SpecEnd) ::
                         SpecStart(_,_,Linked(Some(_), false, false), _) :: (t3: Text) :: (e2: SpecEnd) :: rest => ok }
  }
  "examples" - new group {
    eg := success

    eg := Example(FormattedString.code("a == b"), success).desc.toXml.toString must startWith("<code")

    eg := ("description" ! ((s: String) => s must_== "description")).body() must beSuccessful

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

    eg := Step.stopOnFail.execute must beSuccessful
    eg := Step.stopOnFail(when = {throw new Exception; true}).execute must beError

    eg := Step(1).execute must beSuccessful
    eg := Step({throw new Exception; 1}).execute must beError

  }

  trait specifications {
    lazy val spec1 = new Specification { def is = "title1".title ^ xonly ^ "text1" }
    lazy val spec2 = new Specification { def is = xonly ^ "title2".title ^ "text2" }
    lazy val content = spec1.content
    lazy val content2 = spec2.content
    lazy val content3 = new Specification { def is = xonly ^ args(include="t1") ^ "title".title ^ "text" }.content
    lazy val content4 = new Specification { def is = args(include="t1") ^ "title".title ^ args(include="t2") ^ "text" }.content
    lazy val parentSpec1 = new Specification { def is = "e1" ^ link(spec1) }
    lazy val parentSpec2 = new Specification { def is = "e1" ^ see(spec2) }
    lazy val parentSpec3 = new Specification { def is = "e1" ^ link(spec1.hide) }
    lazy val parentSpec4 = new Specification { def is = "e1" ^ link(spec1, spec2) }

    trait CustomSpecification extends Specification {
      override def map(fs: =>Fragments) = "title".title ^ fs ^ "end of the spec"
    }
    lazy val content5 = new CustomSpecification { def is = sequential ^ "text" }.content

    def fragments = content.fragments

    def startTextEndFragments(fs: Fragments) = fs.fragments.toList.filter(isText || isSpecStart || isSpecEnd)
  }
}