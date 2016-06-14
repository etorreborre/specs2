package org.specs2.specification.mutable

import org.specs2.ScalaCheck
import org.specs2.matcher._
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.specification.dsl.mutable._

class EffectBlocksSpec extends org.specs2.Specification with ThrownExpectations with ScalaCheck { def is = s2"""

 EffectBlocks is used to track a "tree of effects" where it is
 possible to have one effect creating several other ones.

 The interface is:

  - addBlock to add a "leaf" effect. When executed, this effect will not call EffectBlocks
  - nestBlock to add a "node" effect. When executed, this effect will call EffectBlocks to create more effects

 There are 2 modes of operation:

  - record: addBlock/nestBlock store the effects but after each addition update an "effect path" giving the
    position of the current effect in the tree of effects

  - replay(targetPath): addBlock/nestBlock store the effects and they are being executed only if
    they are on the target path

  xxx   $e1
  yyy   $e2
  n * addBlock + replay plays all effects in order               $$e3
  n nestings creates a path for root + n nodes + leaf node == n+2 nodes   $$e3
  opening a new block starts a new branch                                 $$e4
  nesting effects creates a tree of distinct effect paths                 $$e5

"""

  def e1 = {
    val effects = EffectBlocks()
    val paths = new collection.mutable.ListBuffer[EffectPath]
    val actions = new collection.mutable.ListBuffer[String]

    def addBlock = {
      effects.addBlock
      paths.append(effects.effectPath)
      this
    }

    effects.nestBlock {
      actions.append("open block 0")

      addBlock
      addBlock
    }

    paths.toList must_==
      List(
        EffectPath(0, 0, 0),
        EffectPath(0, 0, 1)
      )

    actions.clear
    effects.replay(EffectPath(0, 0, 1))

    actions.toList must_==
      List(
        "open block 0"
      )
  }

  def e2 = {
    val effects = EffectBlocks()
    val paths = new collection.mutable.ListBuffer[EffectPath]
    val actions = new collection.mutable.ListBuffer[String]

    def addBlock = {
      effects.addBlock
      paths.append(effects.effectPath)
      this
    }

    def action =
      effects.nestBlock {
        actions.append("open block 0")
        effects.nestBlock {
          actions.append("open block 0-0")
          addBlock
          addBlock
        }
        effects.nestBlock {
          actions.append("open block 0-1")
          addBlock
          addBlock
        }
      }

    action

    paths.toList must_==
      List(
        EffectPath(0, 0, 0, 0),
        EffectPath(0, 0, 0, 1),
        EffectPath(0, 0, 1, 0),
        EffectPath(0, 0, 1, 1)
      )

    actions.clear
    effects.replay(EffectPath(0, 0, 1, 1))

    actions.toList must_==
      List(
        "open block 0",
        "open block 0-1"
      )
  }
/*
  def e2 = {
    val effects = EffectBlocks()
    var i = 0
    effects.addBlock(i = i + 1)
    effects.replay(effects.effectPath)

    "the effect is executed once" ==> {
      i must_== 1
    }
  }

  def e3 = prop { n: Int =>
    val effects = EffectBlocks()
    var list = new ListBuffer[Int]

    (1 to n) foreach (i => effects.addBlock(list.append(i)))

    effects.record
    list.toList must_== (1 to n).toList

  }

  def e3 = prop { n: Int =>
    val effects = new EffectBlocks
    var path: EffectPath = new EffectPath()

    nest(n, effects, effects.addBlock(path = effects.effectPath))

    effects.record
    effects.effectPath
    effects.effectPath.path.toList must_== Seq.fill(n + 2)(0)
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

    effects.record
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

    effects.record
    paths.distinct must_== paths

  }
  */

  final def nest(n: Int, effects: EffectBlocks, content: =>Any): Any =
    if (n == 0) content
    else        effects.nestBlock(nest(n - 1, effects, content))

  implicit val tinyInt: Arbitrary[Int] = Arbitrary(Gen.choose(1, 5))
}

