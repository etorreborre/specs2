package org.specs2
package reporter

import org.scalacheck.{ Arbitrary, Shrink, Gen, Prop }
import matcher.ScalazMatchers
import scalaz._
import Scalaz._
import specification._
import LeveledBlocks._

class LevelsFoldSpec extends SpecificationWithJUnit with ScalaCheck 
  with ScalazMatchers with ArbitraryFragments { def is = 
    
  args(xonly=true)^
                                                                                          """
  The LevelsFold trait is used to compute the 'level' of Fragments in a list of 
  Fragments.
                                                                                          """^
  "A simple piece of text has level 0"                                                    ^
  { level(t1) must_== List(0) }                                                           ^
                                                                                          p^
  "A new piece of text must be indented by 1"                                             ^
  { level(t1 ^ t2) must_== List(0, 1) }                                                   ^
                                                                                          p^
  "Examples or text following text must be indented by 1"                                 ^
  { level(t1 ^ ex1 ^ ex2 ^ t2 ^ t3) must_== List(0, 1, 1, 1, 2) }                         ^
                                                                                          p^
  "Consecutive examples must have the same indentation"                                   ^
  { level(t1 ^ ex1) must_== List(0, 1) }                                                  ^
  { level(t1 ^ ex1 ^ ex2) must_== List(0, 1, 1) }                                         ^
  { level(t1 ^ ex1 ^ ex2 ^ t2) must_== List(0, 1, 1, 1) }                                 ^
  { level(t1 ^ ex1 ^ t2 ^ ex2) must_== List(0, 1, 1, 2) }                                 ^
                                                                                          p^
  "Tabs can be used to further indent a fragment"                                         ^
  { level(t1 ^ ex1 ^ t ^ t2 ^ ex2) must_== List(0, 1, 1, 2, 3) }                          ^
  { level(t1 ^ ex1 ^ t(2) ^ t2 ^ ex2) must_== List(0, 1, 1, 3, 4) }                       ^
                                                                                          p^
  "Backtabs can be used to further unindent a fragment"                                   ^
  { level(t1 ^ ex1 ^ bt ^ t2 ^ ex2) must_== List(0, 1, 1, 1, 2) }                         ^
  { level(t1 ^ ex1 ^ bt(2) ^ t2 ^ ex2 ^ bt ^ ex1) must_== List(0, 1, 1, 0, 1, 0, 1) }     ^
                                                                                          p^
  "A paragraph unindents the following fragments by 1"                                    ^
  { level(t1 ^ ex1 ^ p ^ t2 ^ ex2) must_== List(0, 1, 1, 0, 1) }                          ^
                                                                                          p^
  "A end resets the following fragment to zero"                                           ^
  { level(t1 ^ ex1 ^ end ^ t2 ^ ex2) must_== List(0, 1, 0, 0, 1) }                        ^
  { level(t1 ^ ex1 ^ end ^ t1 ^ t2 ^ ex2) must_== List(0, 1, 0, 0, 1, 2) }                ^
                                                                                          p^
  "The LevelBlocks monoid must be associative"                                            !
     isAssociative                                                                        ^
                                                                                          end
  implicit def params = set(maxSize -> 5, minTestsOk -> 1000)

  import Arbitrary._                                                                                       
  implicit val arbitraryBlock: Arbitrary[Block] = Arbitrary {
     for (f <- arbitrary[Fragment]) 
       yield toBlock(f)
  }
  implicit val arbitraryBlocks: Arbitrary[LeveledBlocks] = Arbitrary {
    
    def genBlockLevels(sz: Int) = for {
      l <- Gen.listOfN(sz, arbitrary[Block])
    } yield l.foldMap(LeveledBlocks(_))
    
    def sizedList(sz: Int): Gen[LeveledBlocks] = {
      if (sz <= 0) genBlockLevels(1)
      else genBlockLevels(sz)
    }
    Gen.sized(sz => sizedList(sz))
  }
    
  def fold(fs: Fragments) = LeveledBlocks.foldAll(fs.fragments)
  def level(fs: Fragments) = fold(fs).levels
  
  def t1 = "t1"
  def t2 = "t2"
  def t3 = "t3"
  def ex1 = "e1" ! success
  def ex2 = "e2" ! success
}