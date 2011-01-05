package org.specs2
package text
import matcher._

class EditDistanceSpec extends SpecificationWithJUnit with EditDistance with DataTables { def is =
  "The edit distance should"                                                            ^
    "return 0 if there's no insertions"                                                 ! e1^
    "work on insertions"                                                                ! e2^
    "work on suppressions"                                                              ! e3^
    "work on substitutions"                                                             ! e4^
                                                                                        p^
  "The show distance should"                                                            ^
    "work on insertions"                                                                ! s1^
    "work on suppressions"                                                              ! s2^
    "work on suppressions - 2"                                                          ! s3^
    "work on substitutions"                                                             ! s4^
    "not show any difference for the same string"                                       ! s5^
    "show the differences with another separator"                                       ! s6^
    "show the differences with another separator like <<>>"                             ! s7^
    "show the differences with another separator like <<+"                              ! s8^
    "work on 0-sized strings"                                                           ! s9^
    "work on 1-sized strings"                                                           ! s10^
                                                                                        p^
  "The diff shortener should"                                                           ^
    "not shorten a regular string"                                                      ! d1^
    "shorten a diff before if too many letters"                                         ! d2^
    "shorten a diff even at the end"                                                    ! d3^
    "shorten a diff even at the beginning"                                              ! d4^
    "shorten right and left"                                                            ! d5^
    "shorten in the center"                                                             ! d6^
    "shorten in the center with an empty diff"                                          ! d7^
    "shorten in the center with an empty diff"                                          ! d8^
    "shorten left, center and right"                                                    ! d9^
                                                                                        p^
  "The edit distance algorithm should"                                                  ^
    "not use too much memory on a big string comparison when working with file lines"   ! a1^
    "dont use too much memory on a big string comparison on any type of string"         ! a2^
                                                                                        end
  def e1 = editDistance("kitte", "kitte") must_== 0
  def e2 = editDistance("kitte", "kittei") must_== 1
  def e3 = editDistance("kitten", "kit") must_== 3
  def e4 = editDistance("kitten", "kitsin") must_== 2

  def s1 = (showDistance("kitte", "kittei") must_== ("kitte[]", "kitte[i]")) and
           (showDistance("kitten", "kittein") must_== ("kitte[]n", "kitte[i]n"))
  def s2 = showDistance("kitten", "kit") must_== ("kit[ten]", "kit[]")
  def s3 = showDistance("kit", "kitten") must_== ("kit[]", "kit[ten]")
  def s4 = showDistance("kitten", "kitsin") must_== ("kit[te]n", "kit[si]n")
  def s5 = showDistance("kitte", "kitte") must_== ("kitte", "kitte")
  def s6 = showDistance("kitten", "kitsin", "()") must_== ("kit(te)n", "kit(si)n")
  def s7 = showDistance("kitten", "kitsin", "<<>>") must_== ("kit<<te>>n", "kit<<si>>n")
  def s8 = showDistance("kitten", "kitsin", "<<+") must_== ("kit<<te+n", "kit<<si+n")
  def s9 =
    "a"	 || "b" 		| "result" 			  |>
    "" 	 !! ""	   	! ("", "")    		|
    "" 	 !! "a"	   	! ("[]", "[a]")   |
    "a"  !! ""	   	! ("[a]", "[]")   |
    "" 	 !! "ab"	  ! ("[]", "[ab]")	|
    "ab" !! ""   		! ("[ab]", "[]") 	| { (a: String, b: String, result: (String, String)) =>
      showDistance(a, b) must_== result
    }
  def s10 =
    "a"	|| "b" 		| "result" 			  |>
    "a" !! "a"	  ! ("a", "a")    	|
    "a" !! "b"	  ! ("[a]", "[b]")	|
    "a" !! "bc"	  ! ("[a]", "[bc]") |
    "a" !! "ab"	  ! ("a[]", "a[b]")	| { (a: String, b: String, result: (String, String)) =>
      showDistance(a, b) must_== result
    }

  import DiffShortener._
  def d1 = shorten("abcd") must_== "abcd"
  def d2 = shorten("abcdefghijkl[mn]opqr") must_== "...hijkl[mn]opqr"
  def d3 = shorten("abcdefghijkl[mn]") must_== "...hijkl[mn]"
  def d4 = shorten("[mn]abcdefghijkl") must_== "[mn]abcde..."
  def d5 = shorten("abcdefghijkl[mn]opqrstuv") must_== "...hijkl[mn]opqrs..."
  def d6 = shorten("hijkl[zz]abcdefghijklmno[xx]abcde") must_== "hijkl[zz]ab...no[xx]abcde"
  def d7 = shorten("hijkl[]xxabcdefghijklmno[]xxabcde") must_== "hijkl[]xx...no[]xxabc..."
  def d8 = shorten("abcdef[]ghijkl") must_== "...bcdef[]ghijk..."
  def d9 = shorten("abcdefg[zz]abcdefghijklmno[xx]abcdefg") must_== "...cdefg[zz]ab...no[xx]abcde..."

  val factor = 1000
  def a1 = {
    (editDistance("kitten\n" * factor, "kitsin\n" * factor) must be > 0) and
    (showDistance("kitten\n" * factor, "kitsin\n" * factor)._1.size must be > 0)
  }
  def a2 = {
    (editDistance("kitten" * factor, "kitsin" * factor) must be > 0) and
    (showDistance("kitten" * factor, "kitsin" * factor)._1.size must be > 0)
  }
}
