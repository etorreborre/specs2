package org.specs2
package text

import matcher._
import specification.Grouped
import DiffShortener._

class EditDistanceSpec extends Specification with EditDistance with DataTables with Grouped { def is = s2"""

 The edit distance should
   return 0 if there's no insertions                                                                      ${g1.e1}
   work on insertions                                                                                     ${g1.e2}
   work on suppressions                                                                                   ${g1.e3}
   work on substitutions                                                                                  ${g1.e4}

 The show distance should
   work on insertions                                                                                     ${g2.e1}
   work on suppressions                                                                                   ${g2.e2}
   work on suppressions - 2                                                                               ${g2.e3}
   work on substitutions                                                                                  ${g2.e4}
   not show any difference for the same string                                                            ${g2.e5}
   show the differences with another separator                                                            ${g2.e6}
   show the differences with another separator like <<>>                                                  ${g2.e7}
   show the differences with another separator like <<+                                                   ${g2.e8}
   work on 0-sized strings                                                                                ${g2.e9}
   work on 1-sized strings                                                                                ${g2.e1}

 The diff shortener should
   not shorten a regular string                                                                           ${g3.e1}
   shorten a diff before if too many letters                                                              ${g3.e2}
   shorten a diff even at the end                                                                         ${g3.e3}
   shorten a diff even at the beginning                                                                   ${g3.e4}
   shorten right and left                                                                                 ${g3.e5}
   shorten in the center                                                                                  ${g3.e6}
   shorten in the center with an empty diff                                                               ${g3.e7}
   shorten in the center with an empty diff                                                               ${g3.e8}
   shorten left, center and right                                                                         ${g3.e9}

 The edit distance algorithm should
   not use too much memory on a big string comparison when working with file lines                        ${g4.e1}
   dont use too much memory on a big string comparison on any type of string                              ${g4.e2}
                                                                                                           """

  "edit distance" - new g1 {
    e1 := editDistance("kitte", "kitte")   === 0
    e2 := editDistance("kitte", "kittei")  === 1
    e3 := editDistance("kitten", "kit")    === 3
    e4 := editDistance("kitten", "kitsin") === 2
  }

  "show distance" - new g2 {
    e1 := (showDistance("kitte", "kittei")         === ("kitte[]", "kitte[i]")) and
          (showDistance("kitten", "kittein")       === ("kitte[]n", "kitte[i]n"))
    e2 := showDistance("kitten", "kit")            === ("kit[ten]", "kit[]")
    e3 := showDistance("kit", "kitten")            === ("kit[]", "kit[ten]")
    e4 := showDistance("kitten", "kitsin")         === ("kit[te]n", "kit[si]n")
    e5 := showDistance("kitte", "kitte")           === ("kitte", "kitte")
    e6 := showDistance("kitten", "kitsin", "()")   === ("kit(te)n", "kit(si)n")
    e7 := showDistance("kitten", "kitsin", "<<>>") === ("kit<<te>>n", "kit<<si>>n")
    e8 := showDistance("kitten", "kitsin", "<<+")  === ("kit<<te+n", "kit<<si+n")

    e9 :=
      "a"	 || "b" 		| "result" 			  |>
      "" 	 !! ""	   	! ("", "")    		|
      "" 	 !! "a"	   	! ("[]", "[a]")   |
      "a"  !! ""	   	! ("[a]", "[]")   |
      "" 	 !! "ab"	  ! ("[]", "[ab]")	|
      "ab" !! ""   		! ("[ab]", "[]") 	| { (a: String, b: String, result: (String, String)) =>
        showDistance(a, b) must_== result
      }

    e10 :=
      "a"	|| "b" 		| "result" 			  |>
      "a" !! "a"	  ! ("a", "a")    	|
      "a" !! "b"	  ! ("[a]", "[b]")	|
      "a" !! "bc"	  ! ("[a]", "[bc]") |
      "a" !! "ab"	  ! ("a[]", "a[b]")	| { (a: String, b: String, result: (String, String)) =>
        showDistance(a, b) must_== result
      }
  }

  "shorten diffs" - new g3 with DiffShortener {
    e1 := shorten("abcd")                                  === "abcd"
    e2 := shorten("abcdefghijkl[mn]opqr")                  === "...hijkl[mn]opqr"
    e3 := shorten("abcdefghijkl[mn]")                      === "...hijkl[mn]"
    e4 := shorten("[mn]abcdefghijkl")                      === "[mn]abcde..."
    e5 := shorten("abcdefghijkl[mn]opqrstuv")              === "...hijkl[mn]opqrs..."
    e6 := shorten("hijkl[zz]abcdefghijklmno[xx]abcde")     === "hijkl[zz]ab...no[xx]abcde"
    e7 := shorten("hijkl[]xxabcdefghijklmno[]xxabcde")     === "hijkl[]xx...no[]xxabc..."
    e8 := shorten("abcdef[]ghijkl")                        === "...bcdef[]ghijk..."
    e9 := shorten("abcdefg[zz]abcdefghijklmno[xx]abcdefg") === "...cdefg[zz]ab...no[xx]abcde..."
   }

  "performances" - new g4 {
    val factor = 1000
    e1 := {
      (editDistance("kitten\n" * factor, "kitsin\n" * factor) must be > 0) and
      (showDistance("kitten\n" * factor, "kitsin\n" * factor)._1.size must be > 0)
    }
    e2 := {
      (editDistance("kitten" * factor, "kitsin" * factor) must be > 0) and
      (showDistance("kitten" * factor, "kitsin" * factor)._1.size must be > 0)
    }
  }
}
