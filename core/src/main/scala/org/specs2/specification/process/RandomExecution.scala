package org.specs2
package specification
package process

import java.util.concurrent.atomic.AtomicReference

import core._
import collection.Seqx._
import collection.Iterablex._
import control.LazyValue
import scala.collection.concurrent.{Map => CMap, TrieMap}

/**
 * This trait adds random execution constraints between examples.
 */
trait RandomExecution extends SpecificationStructure {
  override def map(fs: => Fragments) = super.map(addExecutionConstraints(fs))

  /**
   * find sequences of concurrent examples
   */
  private def addExecutionConstraints(fs: Fragments): Fragments = {
    val concurrentSequences = fs.fragments.foldLeft(Vector(Vector[Fragment]())) { (res, cur) =>
      res.updateLast(_ :+ cur).toVector ++
        // start a new section if there is a step
        (if (Fragment.isStep(cur)) Vector(Vector[Fragment]()) else Vector())
    }
    val withConstraints = concurrentSequences.map(addExecutionConstraints)
    Fragments(withConstraints.reduce(_ ++ _): _*)
  }

  /**
   * Define a random order and enforce the new execution order using a map of previous executions
   */
  private def addExecutionConstraints(fragments: Vector[Fragment]): Vector[Fragment] = {
    // scramble all fragments
    val scrambled = fragments.zipWithIndex.scramble
    // map of all executions
    val executions: CMap[Int, LazyValue[Execution]] = new TrieMap()

    fragments.zipWithIndex.map { case (f, i) =>
      if (Fragment.isExample(f)) {
        // collect all previous executions
        val previousExecutions: Seq[(Int, Execution)] =
          scrambled.filter(fi => Fragment.isExample(fi._1)).takeWhile(_._2 != i).map { case (pf, pi) => (pi, pf.execution)}

        f.updateRun { oldRun => (env: Env) =>
          previousExecutions.foreach { case (pi, pexec) =>
            // put the previous executions in a map if not already there
            executions.putIfAbsent(pi, LazyValue(() => pexec.execute(env)))
            // trigger the execution if not already done
            executions(pi).value
          }
          // put this execution in the map if not already there
          executions.putIfAbsent(i, LazyValue(() => f.execution.execute(env)))
          // execute this fragment if not already done
          executions(i).value.result
        }
      } else f
    }
  }
}

