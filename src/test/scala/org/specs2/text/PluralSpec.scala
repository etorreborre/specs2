package org.specs2
package text
import matcher.DataTables
import mutable.Specification

class PluralSpec extends Specification with Plural with DataTables {

  "A string can be pluralized: 'apple'.plural(n)" in {

	  "word"   || "quantity"	| "result"	|>
    "apple"  !! 0 			    ! "apple"	  |
    "apple"  !! 1			      ! "apple"	  |
    "apple"  !! 2			      ! "apples"	|
    "apple"  !! 3		        ! "apples"	| { (word, qty, result) =>
      word.plural(qty) must_== result
    }
  }

  "A integer can be described by a pluralized string: 3 qty 'apple' == 3 apples" in {

    "quantity"   | "description"   | "result"   |>
     0           ! "apple"         ! "0 apple"  |
     1           ! "apple"         ! "1 apple"  |
     2           ! "apple"         ! "2 apples" |
     3           ! "apple"         ! "3 apples" | { (q, desc, result) =>
      q qty desc must_== result
    }
  }

  "A integer can be optionally described by a pluralized string: 3 optQty 'apple' == Some(3 apples)" in {

    "quantity"   | "description"   | "result"                               |>
     0           ! "apple"         ! (None:Option[String])                  |
     1           ! "apple"         ! (Some("1 apple"):Option[String])       |
     2           ! "apple"         ! (Some("2 apples"):Option[String])      |
     3           ! "apple"         ! (Some("3 apples"):Option[String])      |
     { (q, desc, result) =>
      q optQty desc must_== result
    }
  }

  "A integer can be optionally described by a invariant string: 3 OptInvariantQty 'skipped' == Some(3 skipped)" in {

    "quantity"   | "description"   | "result"                               |>
     0           ! "skipped"       ! (None:Option[String])                  |
     1           ! "skipped"       ! (Some("1 skipped"):Option[String])       |
     2           ! "skipped"       ! (Some("2 skipped"):Option[String])      |
     3           ! "skipped"       ! (Some("3 skipped"):Option[String])      |
     { (q, desc, result) =>
      q optInvariantQty desc must_== result
    }
  }
  "A integer can be an ordinal: 3 th == '3rd'" in {
    "quantity" | "ordinal"  |>
      0        ! "0th"       |
      1        ! "1st"      |
      2        ! "2nd"      |
      3        ! "3rd"      |
      4        ! "4th"      | { (q, result) => q.th must_== result }
  }
}