package org.specs2
package text
import matcher.DataTables
import specification._

class PluralSpec extends SpecificationWithJUnit with Plural with DataTables {
  
  def is: Fragments = 
                                                                                          """ 
                                                                                          """^
"""A string can be pluralized: "apple".plural(n)""" ! {

	  "word"   || "quantity"	| "result"	|>
    "apple"  !! 0 			    ! "apple"	  |	
    "apple"  !! 1			      ! "apple"	  |	
    "apple"  !! 2			      ! "apples"	|	
    "apple"  !! 2		        ! "apples"	| { (word, qty, result) =>
      word.plural(qty) must_== result
    }
  }
}