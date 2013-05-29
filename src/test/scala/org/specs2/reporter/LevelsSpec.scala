package org.specs2
package reporter

import org.scalacheck.{ Arbitrary, Gen }
import matcher.InternalScalazMatchers
import scalaz._
import Scalaz._
import specification._
import collection.Seqx._
import Levels._
import specification.FragmentsShow._

class LevelsSpec extends Specification with ScalaCheck with InternalScalazMatchers with ArbitraryFragments with Tags { def is = sequential^ s2"""
  The Levels class is used to compute the 'level' of Fragments in a list of Fragments.                                  
                                                                                                                        
  A simple piece of text has level 0                                                                                  
  ${ level(t1) must_== List(0) }
                                                                                                                        
  A new piece of text must be indented by 1                                                                           
  ${ level(t1 ^ t2) must_== List(0, 1) }
                                                                                                                        
  Examples or text following text must be indented by 1                                                               
  ${ level(t1 ^ ex1 ^ t2 ^ t3) must_== List(0, 1, 1, 2) }
  ${ level(t1 ^ ex1 ^ ex2 ^ t2 ^ t3) must_== List(0, 1, 1, 1, 2) }
                                                                                                                        
  Consecutive examples must have the same indentation                                                                 
  ${ level(t1 ^ ex1) must_== List(0, 1) }
  ${ level(t1 ^ ex1 ^ ex2) must_== List(0, 1, 1) }
  ${ level(t1 ^ ex1 ^ ex2 ^ t2) must_== List(0, 1, 1, 1) }
  ${ level(t1 ^ ex1 ^ t2 ^ ex2) must_== List(0, 1, 1, 2) }
                                                                                                                        
  Tabs can be used to further indent a fragment                                                                       
  ${ level(t1 ^ ex1 ^ t ^ t2 ^ ex2) must_== List(0, 1, 1, 2, 3) }
  ${ level(ex1 ^ t ^ ex2) must_== List(0, 0, 1) }
  ${ level(t1 ^ ex1 ^ t(2) ^ t2 ^ ex2) must_== List(0, 1, 1, 3, 4) }
                                                                                                                        
  Backtabs can be used to further unindent a fragment                                                                 
  ${ level(t1 ^ ex1 ^ bt ^ t2 ^ ex2) must_== List(0, 1, 1, 0, 1) }
  ${ level(t1 ^ t2 ^ ex1 ^ bt(2) ^ t2 ^ ex2) must_== List(0, 1, 2, 2, 0, 1) }
                                                                                                                        
  A paragraph unindents the following fragments by 1                                                                  
  ${ level(t1 ^ ex1 ^ p ^ t2 ^ ex2) must_== List(0, 1, 1, 1, 0, 1) }
  ${ level(t1 ^ ex1 ^ p ^ p ^ t2 ^ ex2 ^ p ^ ex1) must_== List(0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0) }
  ${ level(ex1 ^ p ^ ex2 ^ p) must_== List(0, 0, 0, 0, 0, 0) }
  ${ level(ex1 ^ p ^ ex2 ^ p ^ p ^ ex3) must_== List(0, 0, 0, 0, 0, 0, 0, 0, 0) }
  ${ level(t1 ^ p ^ ex1 ^ p ^ ex2 ^ end) must_== List(0, 1, 1, 0, 0, 0, 0, 0) }
                                                                                                                        
  A end resets the following fragment to zero                                                                         
  ${ level(t1 ^ ex1 ^ end ^ t2 ^ ex2) must_== List(0, 1, 1, 0, 1) }
  ${ level(t1 ^ ex1 ^ end ^ t1 ^ t2 ^ ex2) must_== List(0, 1, 1, 0, 1, 2) }
  ${ level("s".title ^ t1 ^ ex1 ^ end ^ t1) must_== List(0, 1, 1, 0) }
                                                                                                                        
  The LevelMonoid monoid must respect the Monoid laws                                                                 
  ${LevelMonoid[Fragment].isMonoid}
                                                                                                                        
  A tree of fragments can be created from the leveled blocks                                                          
    for start ^ t1 ^ ex1 ^ ex2                                                               ${tree().e1}
    for start ^ t1 ^ ex1 ^ end ^ t2 ^ ex2                                                    ${tree().e2}
    for start ^ t1 ^ ex1 ^ p ^ t2 ^ ex2                                                      ${tree().e3}
    for start ^ t1 ^ ex1 ^ ex2 ^ p ^ t2 ^ ex1 ^ ex2                                          ${tree().e4}
    for start\n t1 ex1 $$ok\n ex2 $$ok                                                       ${tree().e5}

  The indentation of a piece of text is calculated as follows
    if the text is "\n    " then it is the number of spaces after the newline                ${indentation().e1}
    if the text is "\n   \n  hello" then it is the number of spaces starting the text        ${indentation().e2}
                                                                                             """


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
      "|  +- Example(e1)",
      "|  |",
      "|  `- End()",
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

    def e5 = {
      val interpolated = s2"""
      t1
        ex1 $ok
        ex2 $ok
      """
      val mapper = (f: Fragment, parents: Seq[Fragment], level: Int) => {
        f match {
          case Text(text) if text.raw.trim.isEmpty => None
          case Text(text)                          => Some(Text(text.raw.trim))
          case _                                   => Some(f)
        }
      }
      treeMap(interpolated)(mapper) must beDrawnAs(
        "SpecStart(start)",
        "|",
        "`- Text(t1)",
        "   |",
        "   +- Example(ex1)",
        "   |",
        "   +- Example(ex2)",
        "   |",
        "   `- SpecEnd(start)")
    }

    def beDrawnAs(lines: String*) = be_==(lines.mkString("", "\n", "\n")) ^^ {
      tree: Tree[Fragment] => tree.drawTree
    }
  }

  case class indentation() {
    def e1 = Levels.startIndentation("\n    ") === 4
    def e2 = Levels.endIndentation("    \n  hello") === 2
  }

  implicit def params = set(maxSize = 5, minTestsOk = 1000)

  import Arbitrary._

  implicit val arbitraryLevel: Arbitrary[Level[Fragment]] = Arbitrary {
    for (f <- arbitrary[Fragment]) yield Levels.fragmentToLevel(f)
  }
  implicit val arbitraryLevels: Arbitrary[Levels[Fragment]] = Arbitrary {

    def genLevels(sz: Int) = for {
      l <- Gen.listOfN(sz, arbitrary[Level[Fragment]])
    } yield l.foldMap(Levels[Fragment](_))

    def sizedList(sz: Int): Gen[Levels[Fragment]] = {
      if (sz <= 0) genLevels(1)
      else genLevels(sz)
    }
    Gen.sized(sz => sizedList(sz))
  }

  def level(fs: Fragments)   (implicit reducer: Reducer[Fragment, Levels[Fragment]] = Levels.FragmentLevelsReducer) = fold(fs.middle)(reducer).levels.map(l => l.level)
  def fold(fs: Seq[Fragment])(implicit reducer: Reducer[Fragment, Levels[Fragment]] = Levels.FragmentLevelsReducer) = fs.reduceWith(reducer)
  def tree(fs: Fragments)    (implicit reducer: Reducer[Fragment, Levels[Fragment]] = Levels.FragmentLevelsReducer) = fold(spec(fs).fragments)(reducer).toTree
  def treeMap(fs: Fragments)(mapper: (Fragment, Seq[Fragment], Int) => Option[Fragment])(implicit reducer: Reducer[Fragment, Levels[Fragment]] = Levels.FragmentLevelsReducer) = fold(spec(fs).fragments)(reducer).toTreeLoc(mapper).toTree
  def spec(fs: Fragments)     = new Specification { def is = "start".title ^ fs }.content

  def t1 = "t1"
  def t2 = "t2"
  def t3 = "t3"
  def ex1 = "e1" ! success
  def ex2 = "e2" ! success
  def ex3 = "e3" ! success
}
