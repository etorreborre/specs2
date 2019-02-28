package org.specs2
package specification
package process

import core._
import collection.Seqx._
import collection.Iterablex._

import scala.collection.concurrent.{TrieMap, Map => CMap}

/**
 * This trait adds random execution constraints between examples.
 *
 * As a result they will be executed in a random sequence
 */
trait RandomSequentialExecution extends SpecificationStructure {
  override def map(fs: =>Fragments, env: Env): Fragments =
    super.map(addExecutionConstraints(fs, env))

  /**
   * find sequences of concurrent examples
   */
  private def addExecutionConstraints(fs: Fragments, env: Env): Fragments = {
    fs.mapFragments { fragments =>
      val concurrentSequences = fragments.foldLeft(Vector(Vector[Fragment]())) { (res, cur) =>
      res.updateLast(_ :+ cur).toVector ++
        // start a new section if there is a step
        (if (Fragment.isStep(cur)) Vector(Vector[Fragment]()) else Vector())
      }
      val withConstraints = concurrentSequences.map(addExecutionConstraints(env))
      withConstraints.reduce(_ ++ _).toList
    }
  }

  /**
   * Define a random order and enforce the new execution order using a map of previous executions
   */
  private def addExecutionConstraints(env: Env)(fragments: Vector[Fragment]): Vector[Fragment] = {
    // scramble all fragments
    val scrambled = fragments.zipWithIndex.scramble(env.random)
    // map of all executions
    val executions: CMap[Int, Execution] = new TrieMap()
    scrambled.foreach { case (pf, i) => executions.putIfAbsent(i, pf.execution) }

    fragments.zipWithIndex.map { case (f, i) =>
      if (Fragment.isExample(f)) {
        val f1 = f.setExecution(executions(i).after(executions.toList.collect { case (j, e) if j < i => e }))
        executions.remove(i)
        f1
      } else f
    }
  }
}
