package org.specs2
package reporter

import specification._

class LevelsFoldSpec extends SpecificationWithJUnit { def is = 
  
                                                                                          """
  The LevelsFold trait is used to compute the 'level' of Fragments in a list of 
  Fragments.
                                                                                          """^
  "The next examples will be indented to 1"                                               ^
  { level(t1) must_== 1 }                                                                 ^
                                                                                          p^
  "The next examples will be indented to 2"                                               ^
  { level(t1 ^ t2) must_== 2 }                                                            ^
  { level(t1 ^ ex1 ^ ex2 ^ t2 ^ t3) must_== 2 }                                           ^
  { level(t1 ^ ex1 ^ t ^ t2 ^ ex2) must_== 2 }                                            ^
                                                                                          p^
  "The next examples will be indented to 1"                                               ^
  { level(t1 ^ ex1) must_== 1}                                                            ^
  { level(t1 ^ ex1 ^ ex2) must_== 1 }                                                     ^
  { level(t1 ^ ex1 ^ ex2 ^ t2) must_== 1 }                                                ^
  { level(t1 ^ ex1 ^ t2 ^ ex2) must_== 1 }                                                ^
  { level(t1 ^ ex1 ^ t ^ t2 ^ ex2 ^ bt ^ ex1) must_== 1 }                                  ^
                                                                                          end
  def fold(fs: Fragments) = LevelsFold.foldAll(fs.fragments)(fs.arguments)
  def level(fs: Fragments) = fold(fs).level
  
  def t1 = "t1"
  def t2 = "t2"
  def t3 = "t3"
  def ex1 = "e1" ! success
  def ex2 = "e2" ! success
}