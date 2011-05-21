package org.specs2
package reporter

import org.scalacheck.{ Arbitrary, Shrink, Gen, Prop }
import matcher.{ ScalazMatchers, Matcher, Expectable, MatchResult, MustExpectable }
import org.specs2.internal.scalaz._
import Scalaz._
import specification._
import Levels._
import FragmentLevelsReducer._
import specification.FragmentsShow._

class LevelsSpec extends Specification with ScalaCheck with ScalazMatchers with ArbitraryFragments { def is = sequential^
                                                                                                                        """
  The Levels class is used to compute the 'level' of Fragments in a list of Fragments.
                                                                                                                        """^p^
  "A simple piece of text has level 0"                                                                                  ^
  { level(t1) must_== List(0) }                                                                                         ^
                                                                                                                        p^
  "A new piece of text must be indented by 1"                                                                           ^
  { level(t1 ^ t2) must_== List(0, 1) }                                                                                 ^
                                                                                                                        p^
  "Examples or text following text must be indented by 1"                                                               ^
  { level(t1 ^ ex1 ^ t2 ^ t3) must_== List(0, 1, 1, 2) }                                                                ^
  { level(t1 ^ ex1 ^ ex2 ^ t2 ^ t3) must_== List(0, 1, 1, 1, 2) }                                                       ^
                                                                                                                        p^
  "Consecutive examples must have the same indentation"                                                                 ^
  { level(t1 ^ ex1) must_== List(0, 1) }                                                                                ^
  { level(t1 ^ ex1 ^ ex2) must_== List(0, 1, 1) }                                                                       ^
  { level(t1 ^ ex1 ^ ex2 ^ t2) must_== List(0, 1, 1, 1) }                                                               ^
  { level(t1 ^ ex1 ^ t2 ^ ex2) must_== List(0, 1, 1, 2) }                                                               ^
                                                                                                                        p^
  "Tabs can be used to further indent a fragment"                                                                       ^
  { level(t1 ^ ex1 ^ t ^ t2 ^ ex2) must_== List(0, 1, 1, 2, 3) }                                                        ^
  { level(t1 ^ ex1 ^ t(2) ^ t2 ^ ex2) must_== List(0, 1, 1, 3, 4) }                                                     ^
                                                                                                                        p^
  "Backtabs can be used to further unindent a fragment"                                                                 ^
  { level(t1 ^ ex1 ^ bt ^ t2 ^ ex2) must_== List(0, 1, 1, 0, 1) }                                                       ^
  { level(t1 ^ t2 ^ ex1 ^ bt(2) ^ t2 ^ ex2) must_== List(0, 1, 2, 2, 0, 1) }                                            ^
                                                                                                                        p^
  "A paragraph unindents the following fragments by 1"                                                                  ^
  { level(t1 ^ ex1 ^ p ^ t2 ^ ex2) must_== List(0, 1, 1, 1, 0, 1) }                                                     ^
  { level(t1 ^ ex1 ^ p ^ p ^ t2 ^ ex2 ^ p ^ ex1) must_== List(0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0) }                        ^
  { level(ex1 ^ p ^ ex2 ^ p) must_== List(0, 0, 0, 0, 0, 0) }                                                           ^
  { level(ex1 ^ p ^ ex2 ^ p ^ p ^ ex3) must_== List(0, 0, 0, 0, 0, 0, 0, 0, 0) }                                        ^
  { level(t1 ^ p ^ ex1 ^ p ^ ex2 ^ end) must_== List(0, 1, 1, 0, 0, 0, 0, 0) }                                          ^
                                                                                                                        p^
  "A end resets the following fragment to zero"                                                                         ^
  { level(t1 ^ ex1 ^ end ^ t2 ^ ex2) must_== List(0, 1, 0, 0, 1) }                                                      ^
  { level(t1 ^ ex1 ^ end ^ t1 ^ t2 ^ ex2) must_== List(0, 1, 0, 0, 1, 2) }                                              ^
  { level("s".title ^ t1 ^ ex1 ^ end ^ t1) must_== List(0, 1, 0, 0) }                                                   ^
                                                                                                                        p^
  "The LevelBlocks monoid must respect the Monoid laws"                                                                 !
    LevelsMonoid.isMonoid                                                                                               ^
                                                                                                                        p^
  "A tree of fragments can be created from the leveled blocks"                                                          ^
    "for start ^ t1 ^ ex1 ^ ex2"                                                                                        ! tree().e1^
    "for start ^ t1 ^ ex1 ^ end ^ t2 ^ ex2"                                                                             ! tree().e2^
    "for start ^ t1 ^ ex1 ^ p ^ t2 ^ ex2"                                                                               ! tree().e3^
    "for start ^ t1 ^ ex1 ^ ex2 ^ p ^ t2 ^ ex1 ^ ex2"                                                                   ! tree().e4^
                                                                                                                        end
  

  def tryAppend = {
    implicit val l = Levels.LevelsMonoid[String]
    val (a, b, c) = (Levels(BlockIndent("t")), Levels(BlockTerminal("")), Levels(BlockIndent("t2")))
    val r = l.append(a, l.append(b, c))
    val r2 = l.append(l.append(a, b), c)
    success
  }

  case class tree() {
    def e1 = tree(t1 ^ ex1 ^ ex2) must beDrawnAs(
      "SpecStart(start)",
      "|",
      "`- Text(t1)",
      "   |",
      "   +- Example(e1)",
      "   |",
      "   +- Example(e2)",
      "   |",
      "   `- SpecEnd(start)")

    def e2 = tree(t1 ^ ex1 ^ end ^ t2 ^ ex2) must beDrawnAs(
      "SpecStart(start)",
      "|",
      "+- Text(t1)",
      "|  |",
      "|  `- Example(e1)",
      "|",
      "+- End()",
      "|",
      "`- Text(t2)",
      "   |",
      "   +- Example(e2)",
      "   |",
      "   `- SpecEnd(start)")

    def e3 = tree(t1 ^ ex1 ^ p ^ t2 ^ ex2) must beDrawnAs(
      "SpecStart(start)",
      "|",
      "+- Text(t1)",
      "|  |",
      "|  +- Example(e1)",
      "|  |",
      "|  +- Br()",
      "|  |",
      "|  `- Backtab(1)",
      "|",
      "`- Text(t2)",
      "   |",
      "   +- Example(e2)",
      "   |",
      "   `- SpecEnd(start)")

    def e4 = tree(t1 ^ ex1 ^ ex2 ^ p ^ t2 ^ ex1 ^ ex2) must beDrawnAs(
      "SpecStart(start)",
      "|",
      "+- Text(t1)",
      "|  |",
      "|  +- Example(e1)",
      "|  |",
      "|  +- Example(e2)",
      "|  |",
      "|  +- Br()",
      "|  |",
      "|  `- Backtab(1)",
      "|",
      "`- Text(t2)",
      "   |",
      "   +- Example(e1)",
      "   |",
      "   +- Example(e2)",
      "   |",
      "   `- SpecEnd(start)")

    def beDrawnAs(lines: String*) = be_==(lines.mkString("", "\n", "\n")) ^^ { 
      tree: Tree[Fragment] => tree.drawTree
    }
  }

  implicit def params = set(maxSize -> 5, minTestsOk -> 1000)

  import Arbitrary._                                                                                       
  implicit val arbitraryBlock: Arbitrary[Block[Fragment]] = Arbitrary {
     for (f <- arbitrary[Fragment]) yield f
  }
  implicit val arbitraryBlocks: Arbitrary[Levels[Fragment]] = Arbitrary {
    
    def genBlockLevels(sz: Int) = for {
      l <- Gen.listOfN(sz, arbitrary[Block[Fragment]])
    } yield l.foldMap(Levels[Fragment](_))
    
    def sizedList(sz: Int): Gen[Levels[Fragment]] = {
      if (sz <= 0) genBlockLevels(1)
      else genBlockLevels(sz)
    }
    Gen.sized(sz => sizedList(sz))
  }

  def fold(fs: Seq[Fragment]) = fs.foldMap(unit)(implicitly[Foldable[Seq]], LevelsConcatMonoid[Fragment])
  def level(fs: Fragments) = fold(fs.middle).levels
  def tree(fs: Fragments) = fold(spec(fs).fragments).toTree
  def spec(fs: Fragments) = new Specification { def is = "start".title ^ fs }.content

  import StandardFragments._
  def isNotFormatting = (f: Tree[Fragment]) => f.rootLabel match {
    case Br() => false
    case Tab(_) => false
    case Backtab(_) => false
    case _ => true
  }
  def t1 = "t1"
  def t2 = "t2"
  def t3 = "t3"
  def ex1 = "e1" ! success
  def ex2 = "e2" ! success
  def ex3 = "e3" ! success
}
