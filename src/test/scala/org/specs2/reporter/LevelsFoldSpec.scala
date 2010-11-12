package org.specs2
package reporter

import org.scalacheck.{ Arbitrary, Gen, Prop }
import scalaz._
import Scalaz._
import specification._
import LeveledBlocks._

class LevelsFoldSpec extends SpecificationWithJUnit with ScalaCheck with ArbitraryFragments { def is = 
  
                                                                                          """
  The LevelsFold trait is used to compute the 'level' of Fragments in a list of 
  Fragments.
                                                                                          """^
  "The next examples will be indented to 1"                                               ^
  { level(t1) must_== List(0) }                                                           ^
                                                                                          p^
  "The next examples will be indented to 2"                                               ^
  { level(t1 ^ t2) must_== List(0, 1) }                                                   ^
  { level(t1 ^ ex1 ^ ex2 ^ t2 ^ t3) must_== List(0, 1, 1, 1, 2) }                         ^
  { level(t1 ^ ex1 ^ t ^ t2 ^ ex2) must_== List(0, 1, 1, 2, 3) }                          ^
                                                                                          p^
  "The next examples will be indented to 1"                                               ^
  { level(t1 ^ ex1) must_== List(0, 1) }                                                  ^
  { level(t1 ^ ex1 ^ ex2) must_== List(0, 1, 1) }                                         ^
  { level(t1 ^ ex1 ^ ex2 ^ t2) must_== List(0, 1, 1, 1) }                                 ^
  { level(t1 ^ ex1 ^ t2 ^ ex2) must_== List(0, 1, 1, 2) }                                 ^
  { level(t1 ^ ex1 ^ t ^ t2 ^ ex2 ^ bt ^ ex1) must_== List(0, 1, 1, 2, 3, 3, 2) }         ^
                                                                                          p^
  "The LevelBlocks monoid must be associative"                                            !
   check { (b1: LeveledBlocks, b2: LeveledBlocks, b3: LeveledBlocks) => 
    (b1 |+| b2) |+| b3 must_== b1 |+| (b2 |+| b3) }                                       ^
                                                                                          end
  implicit def params = set(maxSize -> 3)

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