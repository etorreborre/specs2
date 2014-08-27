package org.specs2.specification.mutable

import org.specs2.ScalaCheck
import org.specs2.matcher.{Matcher, ThrownExpectations}
import scala.collection.mutable.ListBuffer
import org.scalacheck.{Gen, Arbitrary}
import org.specs2.specification.dsl.mutable.{EffectPath, EffectBlocks}

class EffectBlocksSpec extends org.specs2.Specification with ThrownExpectations with ScalaCheck { def is = s2"""

  addBlock + replay plays the effect                                      $e1
  n * addBlock + replay plays all effects in order                        $e2
  n nestings creates a path for root + n nodes + leaf node == n+2 nodes   $e3
  opening a new block starts a new branch                                 $e4
  nesting effects creates a tree of distinct effect paths                 $e5
  stop effects will stop executing effects whenever it is called          $e6
  isAt(effectPath) returns true if we are at the given path               $e7

"""

  def e1 = {
    val effects = new EffectBlocks
    var i = 0
    effects.addBlock(i = i + 1)
    i must_== 0

    effects.replay
    i must_== 1
  }

  def e2 = { prop { n: Int =>
    val effects = new EffectBlocks
    var list = new ListBuffer[Int]

    (1 to n) foreach (i => effects.addBlock(list.append(i)))

    effects.replay
    list.toList must_== (1 to n).toList

  }}

  def e3 = prop { n: Int =>
    val effects = new EffectBlocks
    var path: EffectPath = new EffectPath()

    nest(n, effects, effects.addBlock(path = effects.effectPath))

    effects.replay
    path.path.toList must_== Seq.fill(n + 2)(0)
  }

  def e4 = {
    val effects = new EffectBlocks
    var lastPath: EffectPath = new EffectPath()

    effects.nestBlock {
      effects.addBlock { lastPath = effects.effectPath }
    }
    effects.nestBlock {
      effects.addBlock { lastPath = effects.effectPath }
    }

    effects.replay
    lastPath.path.toList must_== List(0, 1, 0)

  }

  def e5 = {
    val effects = new EffectBlocks
    val paths = new collection.mutable.ListBuffer[EffectPath]

    effects.nestBlock {
      effects.addBlock { paths.append(effects.effectPath) }
      effects.addBlock { paths.append(effects.effectPath) }
    }
    effects.nestBlock {
      effects.nestBlock {
        effects.addBlock { paths.append(effects.effectPath) }
        effects.addBlock { paths.append(effects.effectPath) }
        effects.addBlock { paths.append(effects.effectPath) }
      }
      effects.addBlock { paths.append(effects.effectPath) }
      effects.addBlock { paths.append(effects.effectPath) }
    }

    effects.replay
    paths.distinct must_== paths

  }

  def e6 = {
    val effects = new EffectBlocks
    val paths = new collection.mutable.ListBuffer[EffectPath]

    effects.nestBlock {
      effects.addBlock { paths.append(effects.effectPath) }
      effects.addBlock { paths.append(effects.effectPath) }
    }
    effects.nestBlock {
      effects.nestBlock {
        effects.addBlock { paths.append(effects.effectPath) }
        effects.addBlock { effects.stopEffects }
        effects.addBlock { paths.append(effects.effectPath) }
      }
      effects.addBlock { paths.append(effects.effectPath) }
      effects.addBlock { paths.append(effects.effectPath) }
    }

    effects.replay
    paths must haveSize(3)
  }

  def e7 = {
    val effects = new EffectBlocks

    effects.nestBlock {
      effects.addBlock {}
      effects.addBlock {}
    }
    effects.nestBlock {
      effects.addBlock {}
      effects.addBlock {}
      effects.nestBlock {
        effects.addBlock {}
        effects.addBlock { effects.stopEffects } // we stop exactly here
        effects.addBlock {}
      }
    }

    effects.replay
    effects must beAt(0, 1, 2, 1)
  }

  def beAt(path: Int*): Matcher[EffectBlocks] = (effects: EffectBlocks) =>
    (effects.isAt(Some(EffectPath(path))), s"effect path is ${effects.effectPath} but should be $path")

  final def nest(n: Int, effects: EffectBlocks, content: =>Any): Any =
    if (n == 0) content
    else        effects.nestBlock(nest(n - 1, effects, content))

  implicit val tinyInt: Arbitrary[Int] = Arbitrary(Gen.choose(1, 5))
}

