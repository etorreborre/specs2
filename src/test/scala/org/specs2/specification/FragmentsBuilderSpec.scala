package org.specs2
package specification
import text._

class FragmentsBuilderSpec extends SpecificationWithJUnit {  def is =
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

   * `See` fragments to create a link to another specification                                                          """^p^
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
                                                                                                                        endp^
                                                                                                                        """
How to create an Example
========================                                                                                                """^
                                                                                                                        br^
  "An example is simply created with `string ! e1` where e1 returns a `Result`"                                         ! ex().e1^
  "An example can also use its own description to compute the Result to return"                                         ! ex().e2^
  "An example can have its description marked as `code` for nice html rendering"                                        ! ex().e3^
  "An example has a `matches` method to match its description against a regexp"                                         ^
    "it returns true if there is a match"                                                                               ! ex().matches1^
    "it works even if there are newlines in the description"                                                            ! ex().matches2^endp^
                                                                                                                        """
Other elements
==============                                                                                                          """^
                                                                                                                        br^
  "Text is created by appending other fragments before or after"                                                        ! other().e1^
  "But it can also be defined by itself"                                                                                ! other().e2^
                                                                                                                        end

  case class startEnd() {
    lazy val content = new Specification { def is = "title".title ^ xonly ^ "text" }.content
    lazy val content2 = new Specification { def is = xonly ^ "title".title ^ "text" }.content

    def fragments = content.fragments
    def e1 = (fragments.head must haveClass[SpecStart]) and (fragments.last must haveClass[SpecEnd])
    def e2 = content.start.arguments.xonly
    def e3 = content.start.name must beTheSameAs(content.end.name)
    def e4 = content.start.name.toString must_== "title"
    def e5 = content.fragments.map(_.toString) must contain(lazyfy("SpecStart(title)")).exactlyOnce
    def e6 = content.start.arguments.xonly must beTrue
    def e7 = (content2.start.name.toString must_== "title") and (content2.start.arguments.xonly must beTrue)
  }

  case class ex() {
    def e1 = success

    def e2 = ("name: eric, age: 18" ! function).body().isSuccess
    def function = (s: String) => {
      val Exp = "name: (\\w*), age: (\\d*)".r
      val Exp(name, age) = s
      (name must_== "eric") and (age must_== "18")
    }

    def e3 = Example(CodeMarkup("a == b"), success).desc.toHtml must startWith("<code")

    def matches1 = ("Eric" ! success).matches("E.*")
    def matches2 = ("Eric\nT." ! success).matches("E.*T.*")
  }
  case class other() {
    def e1 = ("t" ^ "t2").middle must have size(2)
    def e2 = ("t":Fragments).middle must have size(1)
  }
}