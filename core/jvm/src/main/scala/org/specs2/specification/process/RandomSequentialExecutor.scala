package org.specs2
package specification
package process

import control._
import core._
import collection.Seqx._
import collection.Iterablex._
import producer._, Producer._
import fp.syntax._
import main.Arguments

import scala.collection.concurrent.{TrieMap, Map => CMap}

/**
 * This trait adds random execution constraints between examples.
 *
 * As a result they will be executed in a random sequence
 */
case class RandomSequentialExecutor(env: Env) extends Executor:
  /**
   * find sequences of concurrent examples in between steps
   * and scramble them
   */
  def execute(specArguments: Arguments): AsyncTransducer[Fragment, Fragment] = { (contents: AsyncStream[Fragment]) =>
    val executed = forceRandomSequentialExecution(Fragments(contents)).contents
    executed.flatMap(SteppedExecutor(env).executeOnline(specArguments))
  }

  private def forceRandomSequentialExecution(fs: Fragments): Fragments =
    fs.mapFragments { fragments =>
      val concurrentSequences = fragments.foldLeft(Vector(Vector[Fragment]())) { (res, cur) =>
        // isolate steps in their own list of executions
        if Fragment.isStep(cur) then
          val step = cur.updateExecution(_.setErrorAsFatal)
          res :+ Vector(step) :+ Vector()
        else
          res.updateLast(_ :+ cur).toVector
      }
      scrambleExecution(concurrentSequences).toList
    }

  /**
   * Define a random order and enforce the new execution order using a map of previous executions
   */
  private def scrambleExecution(fragments: Vector[Vector[Fragment]]): Vector[Fragment] =
    val executions: CMap[Int, Execution] = new TrieMap()
    val fragmentsList = fragments.flatten
    fragmentsList.zipWithIndex.foreach { case (f, i) => executions.putIfAbsent(i, f.execution) }

    val scrambled = fragments.foldLeft(Vector[Int]()) { case (res, cur) =>
          res ++ (0 until cur.size).scramble(env.random).map(_ + res.size).toVector
        }

    val mustExecuteBefore: CMap[Int, Seq[Int]] = new TrieMap()
    scrambled.zipWithIndex.foreach { case (originalIndex, newIndex) => mustExecuteBefore.putIfAbsent(originalIndex, scrambled.take(newIndex)) }

    mustExecuteBefore.toList.sortBy(_._2.size).foreach { case (n, beforeIndices) =>
      val before = beforeIndices.map { case i => executions(i) }.toList
      executions.put(n, executions(n).after(before).startExecution(env))
    }
    fragmentsList.zipWithIndex.map { case (f, i) =>
      f.setExecution(executions(i))
    }
