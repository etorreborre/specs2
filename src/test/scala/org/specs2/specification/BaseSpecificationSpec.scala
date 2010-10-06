package org.specs2
package specification
import reporter._

class BaseSpecificationSpec extends Specification {
   val content = 
"""A basic specification is just some pieces of text interleaved with some Fragments
   This is an example of such a specification:
   
     object ReverseSpec extends Specification { 
       val content = 
       "A reverse function can reverse a string"^
       "  if the string is empty, it return an empty string" ! e1
       "  if the string is not empty, it returns the reversed string" ! e2
      
       def e1 = reverse("") must_== ""
       def e2 = reverse("abc") must_== "bca"
    
   When execute on the Console, the expected output is: 
   > Specification Reverse
   > A reverse function can reverse a string
   >   + if the string is empty, it return an empty string
   >   + if the string is not empty, it returns the reversed string
    
  The following Fragments are going to specify the basic components for such a specification:
    * building the list of Fragments, with text and executable code
    * executing the Fragments to collect results
    * printing out results to the Console
"""                                                      ^
"  Fragments building specification"                     ^
   include(new ExamplesSpec)                             ^
"  Fragments execution"                                  ^
   include(new FragmentsExecutionSpec)                   ^
"  Fragments execution"                                  ^
   include(new ConsoleReporterSpec)
}