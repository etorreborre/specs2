package org.specs2
package specification

import text._
import execute.Skipped
import matcher._

class FragmentsBuilderSpec extends Specification with ResultMatchers with Groups {  def is =
                                                                                                                        """
  In a Specification, the `contents` variable stores an instance of the Fragments class,
  which is merely a list of fragments. Those fragments are:

   * `SpecStart` / `SpecEnd` elements. A `SpecStart` contains the arguments applicable to all the specification
     fragments, and an identifier for the Specification:`SpecName`. The corresponding `SpecEnd` fragment must have the
     same name

   * `Text` elements which are simply embedding a String

   * `Example` elements. An Example has:
      + a description which can contain some markup tags to render it as code for example
      + an executable block returning a `Result`: a `Success`, a `Failure`

   * `Step` fragments which are not reported but execute an action

                                                                                                                        """^p^
                                                                                                                        """
SpecStart/SpecEnd
=================                                                                                                       """^
                                                                                                                        br^
  "In a specification SpecStart and end fragments are automatically added"                                              ^
    "SpecStart is always the first fragment and SpecEnd the last"                                                       ! g1().e1^
    "the SpecStart object contains the specification arguments"                                                         ! g1().e2^
    "SpecStart and SpecEnd have the same name"                                                                          ! g1().e3^
    "The SpecStart element can be created by adding a title to the specification"                                       ! g1().e4^
    "When a title is created there is only one SpecStart in the specification"                                          ! g1().e5^
    "A title can be added before arguments are declared"                                                                ! g1().e6^
    "A title can be added after arguments are declared"                                                                 ! g1().e7^
    "A title can not be empty"                                                                                          ! g1().e8^
    "Arguments can be added in different place in the spec"                                                             ^
      "new Arguments values are added to the existing ones"                                                             ! g1().e9^
      "and override them if already declared"                                                                           ! g1().e10^
      "it also works with the map method in BaseSpecification"                                                          ! g1().e11^
                                                                                                                        endp^
    "A specification can be linked"                                                                                     ^
      "and included"                                                                                                    ! g1().e12^
      "or just referenced"                                                                                              ! g1().e13^
      "and hidden, it'll be executed but not reported"                                                                  ! g1().e14^
                                                                                                                        p^
    "Several specifications can be linked at once"                                                                      ! g1().e15^
    "A specification can reference itself without creating a loop"                                                      ! g1().e16^
                                                                                                                        endp^
                                                                                                                        """
How to create an Example
========================                                                                                                """^
                                                                                                                        br^
  "An example is simply created with `string ! e1` where e1 returns a `Result`"                                         ! g2().e1^
  "An example can have its description marked as `code` for nice html rendering"                                        ! g2().e2^
  "An example can use its own description"                                                                              ! g2().e3^
  "An example can use a partial function to extract values from its text"                                               ! g2().e4^
    "the description must be stripped out of value markers"                                                             ! g2().e5^
                                                                                                                        p^
  "An Error in the Example body will not fail the example creation"                                                     ! g2().e6^
    "even if it is an AssertionError"                                                                                   ! g2().e7^
                                                                                                                        p^
  "An example has a `matches` method to match its description against a regexp"                                         ^
    "it returns true if there is a match"                                                                               ! g3().e1^
    "it works even if there are newlines in the description"                                                            ! g3().e2^
                                                                                                                        endp^
                                                                                                                        """
Other elements
==============                                                                                                          """^
                                                                                                                        br^
  "A Fragments object can be created by appending fragment objects"                                                     ! g4().e1^
  "Or simply by casting a String to a Fragments object"                                                                 ! g4().e2^
                                                                                                                        endp^
  "A Step can be created from an Either[Result, T]"                                                                     ^
    "from a Left(Skipped())"                                                                                            ! g4().e3^
    "from a Right(value)"                                                                                               ! g4().e4^
                                                                                                                        endp^
  "A Step can be created with a stopOnFail value"                                                                       ^
    "with a stopOnFail = true"                                                                                          ! g4().e5^
    "with a stopOnFail value throwing an exception"                                                                     ! g4().e6^
                                                                                                                        endp^
  "A Step can be created with an action"                                                                                ^
    "executing ok"                                                                                                      ! g4().e7^
    "throwing an exception"                                                                                             ! g4().e8^
                                                                                                                        end

  "start and end" - new g1 with specifications {
    e1  := (fragments.head must haveClass[SpecStart]) and (fragments.last must haveClass[SpecEnd])
    e2  := content.specStart.arguments.xonly
    e3  := content.specStart.specName must beTheSameAs(content.specEnd.specName)
    e4  := content.specStart.title must_== "title"
    e5  := content.fragments.map(_.toString) must contain(lazyfy("SpecStart(title)")).exactlyOnce
    e6  := content.specStart.arguments.xonly must beTrue
    e7  := (content2.specStart.title must_== "title") and (content2.specStart.arguments.xonly must beTrue)
    e8  :=  content6.specStart.title must not(beEmpty)
    e9  := (content3.specStart.arguments.xonly must beTrue) and (content3.specStart.arguments.include must_== "t1")
    e10 := content4.specStart.arguments.include must_== "t2"
    e11 := content5.specStart.arguments.sequential must beTrue

    e12 := parentSpec1.content.fragments.toList must
              beLike { case SpecStart(_,_,_) :: Text(_) :: SpecStart(_,_,Linked(Some(l), false, false)) :: rest => ok }
    e13 := parentSpec2.content.fragments.toList must
              beLike { case SpecStart(_,_,_) :: Text(_) :: SpecStart(_,_,Linked(Some(l), true, false)) :: rest => ok }
    e14 := parentSpec3.content.fragments.toList must
              beLike { case SpecStart(_,_,_) :: Text(_) :: SpecStart(_,_,Linked(Some(l), false, true)) :: rest => ok }
    e15 := parentSpec4.content.fragments.toList must
              beLike { case SpecStart(_,_,_) :: Text(_) ::
                         SpecStart(_,_,Linked(Some(_), false, false)) :: Text(_) :: SpecEnd(_,_) ::
                         SpecStart(_,_,Linked(Some(_), false, false)) :: Text(_) :: SpecEnd(_,_) :: rest => ok }

    e16 := selfReferencing.content must terminate(retries = 3, sleep = 100.millis)
  }

  "examples" - new g2 {
    e1 := success

    e2 := Example(CodeMarkup("a == b"), success).desc.toHtml must startWith("<code")

    e3 := ("description" ! ((s: String) => s must_== "description")).body() must beSuccessful

    val soExample = "given the name: ${eric}, then the age is ${18}" ! so {
      case (name: String, age: String) => age.toInt must_== 18
    }
    e4 := soExample.body() must beSuccessful
    e5 := soExample.desc.toString must_== "given the name: eric, then the age is 18"

    def execute = FragmentExecution.executeFragment(args())
    e6 := execute("example" ! { throw new NoSuchMethodError("flushBuffer"); success }).toString must beMatching(".*ThrowableException.*NoSuchMethodError\\: flushBuffer.*")
    e7 := execute("example" ! { throw new AssertionError(); success }).toString must not contain("Fragment evaluation error")
  }

  "examples matches" - new g3 {
    e1 := ("Eric" ! success).matches("E.*")
    e2 := ("Eric\nT." ! success).matches("E.*T.*")
  }

  "other elements" - new g4 {
    e1 := ("t" ^ "t2").middle must have size(2)
    e2 := ("t":Fragments).middle must have size(1)
    e3 := Step.fromEither(Left(Skipped())).execute must beSkipped
    e4 := Step.fromEither(Right("value")).execute must beSuccessful
    e5 := Step(stopOnFail = true).execute must beSuccessful
    e6 := Step(stopOnFail = {throw new Exception; true}).execute must beError
    e7 := Step(1).execute must beSuccessful
    e8 := Step({throw new Exception; 1}).execute must beError
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
    lazy val content6 = new Specification {
      val number: Given[Int] = (_:String).toInt
      def is = "a number ${0}" ^ number
    }.content

    def fragments = content.fragments
  }
}