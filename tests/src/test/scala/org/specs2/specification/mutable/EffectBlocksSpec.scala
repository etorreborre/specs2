package org.specs2.specification.mutable

import org.specs2.ScalaCheck
import org.specs2.matcher._
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.specification.dsl.mutable._
import org.specs2.data.Trees._

class EffectBlocksSpec extends org.specs2.Specification with ThrownExpectations with ScalaCheck { def is = s2"""

 EffectBlocks is used to track a "tree of effects" where it is
 possible to have one effect creating several other ones.

 The interface is:

  - addBlock to add a "leaf". This just increments the current "effect path"
  - nestBlock to add a "node" effect. When executed, this effect will potentially create more effects

 There are 2 modes of operation:

  - record: addBlock/nestBlock store the effects but after each addition update an "effect path" giving the
    position of the current effect in the tree of effects

  - replay(targetPath): addBlock/nestBlock store the effects and they are being executed only if
    they are on the target path

  example1   $e1
  example2   $e2
  for any nesting and any path in the effects tree only actions on the path are executed $e3
"""

  def e1 = {
    val effects = EffectBlocks()
    val actions = new collection.mutable.ListBuffer[String]

    def addBlock = {
      effects.addBlock("")
      this
    }

    effects.nestBlock {
      actions.append("open block 0")
      addBlock
      addBlock
    }

    effects.record

    effects.paths must_==
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
    val actions = new collection.mutable.ListBuffer[String]

    def addBlock = {
      effects.addBlock("")
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
    effects.record

    effects.paths must_==
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

  def e3 = prop { actions: Actions =>

    val effectBlocks = EffectBlocks()
    val results = new collection.mutable.ListBuffer[String]

    actions.run(effectBlocks, results)

    val paths = effectBlocks.blocksTree.root.tree.allPaths
    val path: EffectPath = Gen.oneOf(paths).sample.map(p => EffectPath(p:_*)).getOrElse(EffectPath())

    results.clear
    effectBlocks.replay(path)

    results.toList must_==
      actions.getActionsOnPath(path)
  }

  implicit val tinyInt: Arbitrary[Int] = Arbitrary(Gen.choose(1, 5))
}

private[specs2]
case class Actions(nested: List[Actions] = List(), name: String) {

  def getActionsOnPath(path: EffectPath): List[String] =
    path.path.drop(1).toList match {
      case Nil => Nil
      case n :: rest => nested(n).name :: getActionsOnPath(EffectPath(rest:_*))
    }

  def run(effects: EffectBlocks, actions: collection.mutable.ListBuffer[String]): Unit = {
    if (nested.isEmpty) {
      effects.addBlock(name)
      actions.append(name)
    }
    else
      effects.nestBlock {
        actions.append(name)
        effects.addBlock(name)
      nested.foreach(_.run(effects, actions))
    }
    ()
  }

}

object Actions {
  implicit def ArbitraryActions: Arbitrary[Actions] = Arbitrary {
    Gen.const(Actions(name = ""))
  }
}

