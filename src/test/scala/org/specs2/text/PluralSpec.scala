package org.specs2
package text
import matcher.DataTables
import specification._

class PluralSpec extends SpecificationWithJUnit with Plural with DataTables { def is =
  
  "A string can be pluralized: 'apple'.plural(n)" ! {

	  "word"   || "quantity"	| "result"	|>
    "apple"  !! 0 			    ! "apple"	  |	
    "apple"  !! 1			      ! "apple"	  |	
    "apple"  !! 2			      ! "apples"	|	
    "apple"  !! 3		        ! "apples"	| { (word, qty, result) =>
      word.plural(qty) must_== result
    }
  }                                                                                       ^ 
                                                                                          p^
  "A integer can be described by a pluralized string: 3 qty 'apple' == 3 apples" ! {

    "quantity"   | "description"   | "result"   |>
     0           ! "apple"         ! "0 apple"  | 
     1           ! "apple"         ! "1 apple"  |
     2           ! "apple"         ! "2 apples" |
     3           ! "apple"         ! "3 apples" | { (q, desc, result) =>
      q qty desc must_== result
    }
  }                                                                                       ^
                                                                                          p^
  "A integer can be optionally described by a pluralized string:"+
  "3 qty 'apple' == Some(3 apples)" ! {

    "quantity"   | "description"   | "result"                               |>
     0           ! "apple"         ! (None:Option[String])                  | 
     1           ! "apple"         ! (Some("1 apple"):Option[String])       |
     2           ! "apple"         ! (Some("2 apples"):Option[String])      |
     3           ! "apple"         ! (Some("3 apples"):Option[String])      | 
     { (q, desc, result) =>
      q optQty desc must_== result
    }
  }
}