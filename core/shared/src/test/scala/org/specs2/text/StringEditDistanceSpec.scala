package org.specs2
package text

import matcher.{DataTables, TypedEqual, Matcher}
import Matcher.*

class StringEditDistanceSpec extends Specification with StringEditDistance with DataTables with TypedEqual {
  def is = s2"""

 The edit distance should
   return 0 if there's no insertions                                                                      $edit1
   work on insertions                                                                                     $edit2
   work on suppressions                                                                                   $edit3
   work on substitutions                                                                                  $edit4

 The show distance should
   work on insertions                                                                                     $show1
   work on suppressions                                                                                   $show2
   work on suppressions - 2                                                                               $show3
   work on substitutions                                                                                  $show4
   not show any difference for the same string                                                            $show5
   show the differences with another separator                                                            $show6
   show the differences with another separator like <<>>                                                  $show7
   show the differences with another separator like <<+                                                   $show8
   work on 0-sized strings                                                                                $show9
   work on 1-sized strings                                                                                $show10
   work on a swap                                                                                         $show11

 The diff shortener should
   not shorten a regular string                                                                           $diff1
   shorten a diff before if too many letters                                                              $diff2
   shorten a diff even at the end                                                                         $diff3
   shorten a diff even at the beginning                                                                   $diff4
   shorten right and left                                                                                 $diff5
   shorten in the center                                                                                  $diff6
   shorten in the center with an empty diff                                                               $diff7
   shorten in the center with an empty diff                                                               $diff8
   shorten left, center and right                                                                         $diff9

 The edit distance algorithm should
   not use too much memory on a big string comparison when working with file lines                        $memory1
   not use too much memory on a big string comparison on any type of string                               $memory2

"""

  def edit1 = editDistance("kitte", "kitte") === 0
  def edit2 = editDistance("kitte", "kittei") === 1
  def edit3 = editDistance("kitten", "kit") === 3
  def edit4 = editDistance("kitten", "kitsin") === 2

  def show1 =
    (showDistance("kitte", "kittei") === "kitte[]" -> "kitte[i]") and
      (showDistance("kitten", "kittein") === "kitte[]n" -> "kitte[i]n")

  def show2 = showDistance("kitten", "kit") === "kit[ten]" -> "kit[]"
  def show3 = showDistance("kit", "kitten") === "kit[]" -> "kit[ten]"
  def show4 = showDistance("kitten", "kitsin") === "kit[te]n" -> "kit[si]n"
  def show5 = showDistance("kitte", "kitte") === "kitte" -> "kitte"
  def show6 = showDistance("kitten", "kitsin", "()") === "kit(te)n" -> "kit(si)n"
  def show7 = showDistance("kitten", "kitsin", "<<>>") === "kit<<te>>n" -> "kit<<si>>n"
  def show8 = showDistance("kitten", "kitsin", "<<+") === "kit<<te+n" -> "kit<<si+n"

  def show9 =
    "a" || "b" | "result" |>
      "" !! "" ! "" -> "" |
      "" !! "a" ! "[]" -> "[a]" |
      "a" !! "" ! "[a]" -> "[]" |
      "" !! "ab" ! "[]" -> "[ab]" |
      "ab" !! "" ! "[ab]" -> "[]" | { (a: String, b: String, result: (String, String)) =>
        showDistance(a, b) must ===(result)
      }

  def show10 =
    "a" || "b" | "result" |>
      "a" !! "a" ! "a" -> "a" |
      "a" !! "b" ! "[a]" -> "[b]" |
      "a" !! "bc" ! "[a]" -> "[bc]" |
      "a" !! "ab" ! "a[]" -> "a[b]" | { (a: String, b: String, result: (String, String)) =>
        showDistance(a, b) must ===(result)
      }

  def show11 =
    showDistance("abcd", "acbd") === "a[bc]d" -> "a[cb]d"

  def diff1 = shorten("abcd") === "abcd"
  def diff2 = shorten("abcdefghijkl[mn]opqr") === "...hijkl[mn]opqr"
  def diff3 = shorten("abcdefghijkl[mn]") === "...hijkl[mn]"
  def diff4 = shorten("[mn]abcdefghijkl") === "[mn]abcde..."
  def diff5 = shorten("abcdefghijkl[mn]opqrstuv") === "...hijkl[mn]opqrs..."
  def diff6 = shorten("hijkl[zz]abcdefghijklmno[xx]abcde") === "hijkl[zz]ab...no[xx]abcde"
  def diff7 = shorten("hijkl[]xxabcdefghijklmno[]xxabcde") === "hijkl[]xx...no[]xxabc..."
  def diff8 = shorten("abcdef[]ghijkl") === "...bcdef[]ghijk..."
  def diff9 = shorten("abcdefg[zz]abcdefghijklmno[xx]abcdefg") === "...cdefg[zz]ab...no[xx]abcde..."

  val factor = 1000
  def memory1 =
    (editDistance("kitten\n" * factor, "kitsin\n" * factor) must be_>(0)) and
      (showDistance("kitten\n" * factor, "kitsin\n" * factor)._1.length must be_>(0))

  def memory2 =
    (editDistance("kitten" * factor, "kitsin" * factor) must be_>(0)) and
      (showDistance("kitten" * factor, "kitsin" * factor)._1.length must be_>(0))
}
