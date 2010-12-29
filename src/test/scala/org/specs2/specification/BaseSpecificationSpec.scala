package org.specs2
package specification
import reporter._

class BaseSpecificationSpecSuite extends Specification {  def is = 
                                                                                          """
 A Specification is just some pieces of text interleaved with executable pieces of code.
 This is an example of such a specification:
 
    class ReverseSpec extends Specification { def is =
      "A reverse function can reverse a string"                      ^
        "if the string is empty, it return an empty string"          ! e1
        "if the string is not empty, it returns the reversed string" ! e2
    
      def e1 = reverse("") must_== ""
      def e2 = reverse("abc") must_== "bca"
    }

 When executed on the Console, the expected output is:

     > Specification Reverse
     > A reverse function can reverse a string
     > + if the string is empty, it return an empty string
     > + if the string is not empty, it returns the reversed string
  
There are 3 steps in executing a specification:

  1. building a list of Fragments, with text, formatting directives and executable code (Examples)
  2. executing the Fragments to collect results
  3. printing the results to the Console
                                                                                          """^
                                                                                          p^
   include(new ExamplesSpec)                                                              ^
   include(new FragmentsExecutionSpec)                                                    ^
   include(new TextPrinterSpec)                                                           ^
                                                                                          end
}