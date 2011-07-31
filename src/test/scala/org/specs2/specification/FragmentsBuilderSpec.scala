package org.specs2
package specification
import text._
import execute.Skipped
import matcher._

class FragmentsBuilderSpec extends Specification with ResultMatchers {  def is =
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
    "SpecStart is always the first fragment and SpecEnd the last"                                                       ! startEnd().e1^
    "the SpecStart object contains the specification arguments"                                                         ! startEnd().e2^
    "SpecStart and SpecEnd have the same name"                                                                          ! startEnd().e3^
    "The SpecStart element can be created by adding a title to the specification"                                       ! startEnd().e4^
    "When a title is created there is only one SpecStart in the specification"                                          ! startEnd().e5^
    "A title can be added before arguments are declared"                                                                ! startEnd().e6^
    "A title can be added after arguments are declared"                                                                 ! startEnd().e7^
    "Arguments can be added in different place in the spec"                                                             ^
      "new Arguments values are added to the existing ones"                                                             ! startEnd().e8^
      "and override them if already declared"                                                                           ! startEnd().e9^
      "it also works with the map method in BaseSpecification"                                                          ! startEnd().e10^
                                                                                                                        endp^
    "A specification can be linked"                                                                                     ^
      "and included"                                                                                                    ! startEnd().e11^
      "or just referenced"                                                                                              ! startEnd().e12^
                                                                                                                        endp^
                                                                                                                        """
How to create an Example
========================                                                                                                """^
                                                                                                                        br^
  "An example is simply created with `string ! e1` where e1 returns a `Result`"                                         ! ex().e1^
  "An example can have its description marked as `code` for nice html rendering"                                        ! ex().e2^
  "An example can use its own description"                                                                              ! ex().e3^
  "An example can use a partial function to extract values from its text"                                               ! ex().e4^
    "the description must be stripped out of value markers"                                                             ! ex().e5^
                                                                                                                        p^
  "An Error in the Example body will fail the example creation"                                                         ! ex().e6^
     "except if it is an AssertionError"                                                                                ! ex().e7^
                                                                                                                        p^
  "An example has a `matches` method to match its description against a regexp"                                         ^
    "it returns true if there is a match"                                                                               ! ex().matches1^
    "it works even if there are newlines in the description"                                                            ! ex().matches2^endp^
                                                                                                                        """
Other elements
==============                                                                                                          """^
                                                                                                                        br^
  "A Fragments object by appending fragment objects"                                                                    ! other().e1^
  "Or simply by casting a String to a Fragments object"                                                                 ! other().e2^
                                                                                                                        br^
  "A Step can be created from an Either[Result, T]"                                                                     ^
    "from a Left(Skipped())"                                                                                            ! other().e3^
    "from a Right(value)"                                                                                               ! other().e4^
                                                                                                                        end

  case class startEnd() {
    lazy val spec1 = new Specification { def is = "title".title ^ xonly ^ "text" }
    lazy val spec2 = new Specification { def is = xonly ^ "title".title ^ "text" }
    lazy val content = spec1.content
    lazy val content2 = spec2.content
    lazy val content3 = new Specification { def is = xonly ^ args(include="t1") ^ "title".title ^ "text" }.content
    lazy val content4 = new Specification { def is = args(include="t1") ^ "title".title ^ args(include="t2") ^ "text" }.content
    lazy val parentSpec1 = new Specification { def is = "e1" ^ link(spec1) }
    lazy val parentSpec2 = new Specification { def is = "e1" ^ see(spec2) }

    trait CustomSpecification extends Specification {
      override def map(fs: =>Fragments) = "title".title ^ fs ^ "end of the spec"
    }
    lazy val content5 = new CustomSpecification { def is = sequential ^ "text" }.content

    def fragments = content.fragments
    def e1 = (fragments.head must haveClass[SpecStart]) and (fragments.last must haveClass[SpecEnd])
    def e2 = content.start.arguments.xonly
    def e3 = content.start.specName must beTheSameAs(content.end.specName)
    def e4 = content.start.title must_== "title"
    def e5 = content.fragments.map(_.toString) must contain(lazyfy("SpecStart(title)")).exactlyOnce
    def e6 = content.start.arguments.xonly must beTrue
    def e7 = (content2.start.title must_== "title") and (content2.start.arguments.xonly must beTrue)
    def e8 = (content3.start.arguments.xonly must beTrue) and (content3.start.arguments.include must_== "t1")
    def e9 = content4.start.arguments.include must_== "t2"
    def e10 = content5.start.arguments.sequential must beTrue
    def e11 = parentSpec1.content.fragments.toList must
              beLike { case SpecStart(_,_,_,_) :: Text(_) :: SpecStart(_,_,Some(l), false) :: rest => ok }
    def e12 = parentSpec2.content.fragments.toList must
              beLike { case SpecStart(_,_,_,_) :: Text(_) :: SpecStart(_,_,Some(l), true) :: rest => ok }
  }

  case class ex() {
    def e1 = success

    def e2 = Example(CodeMarkup("a == b"), success).desc.toHtml must startWith("<code")

    def e3 = ("description" ! ((s: String) => s must_== "description")).body() must beSuccessful

    val soExample = "given the name: ${eric}, then the age is ${18}" ! so {
      case (name: String, age: String) => age.toInt must_== 18
    }
    def e4 = soExample.body() must beSuccessful
    def e5 = soExample.desc.toString must_== "given the name: eric, then the age is 18"

    def execute = FragmentExecution.executeFragment(args())
    def e6 = execute("example" ! { throw new LinkageError(); success }).toString must contain("Fragment evaluation error")
    def e7 = execute("example" ! { throw new AssertionError(); success }).toString must not contain("Fragment evaluation error")

    def matches1 = ("Eric" ! success).matches("E.*")
    def matches2 = ("Eric\nT." ! success).matches("E.*T.*")
  }
  case class other() {
    def e1 = ("t" ^ "t2").middle must have size(2)
    def e2 = ("t":Fragments).middle must have size(1)
    def e3 = Step.fromEither(Left(Skipped())).execute must beSkipped
    def e4 = Step.fromEither(Right("value")).execute must beSuccessful
  }
}