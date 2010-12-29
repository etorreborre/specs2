package org.specs2
package specification

class ExamplesSpec extends SpecificationWithJUnit {  def is =
                                                                                          """
  In a Specification, the `contents` variable stores an instance of the Fragments class,
  which is merely a list of fragments. Those fragments are:
  
   * `SpecStart` / `SpecEnd` elements. A `SpecStart` contains the arguments applicable to all the specification
     fragments, and an identifier for the Specification (`SpecName`)

   * `Text` elements. Mostly a String, but they can be translated to html using a Markup notation

   * `Example` elements, with an executable block returning a `Result` (a `Success`, a `Failure`,...)

   * `Step` fragments which are not reported but execute an action

   * `See` fragments to create a link to another specification

   How to create an Example
   ========================                                                                      """^
                                                                                                  p^
  "An example is simply created with `string ! e1` where e1 returns a `Result`"                   ! success^
  "An example is can also take its own description to return a Result"                            ! e1^
  "An example has a matches method to match its description against a regexp"                     ^
    "it returns true if there is a match"                                                         !matches().e1^
    "it works even if there are newlines in the description"                                      !matches().e2^
                                                                                                  end
                                                                                        
  def e1 = {
  	val ex = "name: eric, age: 18" ! function
  	ex.body().isSuccess must beTrue
  }
  
  def function = (s: String) => {
    val Exp = "name: (\\w*), age: (\\d*)".r
    val Exp(name, age) = s
    (name must_== "eric") and (age must_== "18")
  }
  
  case class matches() {
    def e1 = ("Eric" ! success).matches("E.*") must beTrue
    def e2 = ("Eric\nT." ! success).matches("E.*T.*") must beTrue
  }
}