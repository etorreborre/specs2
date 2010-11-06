package org.specs2
package reporter

import specification._

class LevelsFoldSpec extends SpecificationWithJUnit { def is = 
  
                                                                                          """
  The LevelsFold trait is used to compute the 'level' of Fragments in a list of 
  Fragments.
                                                                                          """^
  "The next examples will be indented to 1"                                               ^
  { level("t1") must_== 1 }                                                               ^
                                                                                          p^
  "The next examples will be indented to 2"                                               ^
  { level("t1" ^ "t2") must_== 2 }                                                        ^
  { level("t1" ^ "e1"!true ^ "e2"!true ^ "t3" ^ "t4") must_== 2 }                         ^
                                                                                          p^
  "The next examples will be indented to 1"                                               ^
  { level("t1" ^ "e1"!true) must_== 1}                                                    ^
  { level("t1" ^ "e1"!true ^ "e2"!true) must_== 1 }                                       ^
  { level("t1" ^ "e1"!true ^ "e2"!true ^ "t3") must_== 1 }                                ^
                                                                                          end
  def fold(fs: Fragments) = LevelsFold.foldAll(fs.fragments)(fs.arguments)
  def level(fs: Fragments) = fold(fs).level
}