package org.specs2
package scalacheck

import org.scalacheck.util.FreqMap

object PrettyDetails:
  /**
   * Failure details might get collected with the collect operation when evaluating
   * the Prop
   */
  def collectDetails[T](fq: FreqMap[Set[T]]): execute.Details =
    fq.getRatios.flatMap(_._1.toList).collect { t =>
      t.asInstanceOf[Matchable] match
        case d : execute.FailureDetails => d
    }.headOption.getOrElse(execute.NoDetails)

  /**
   * Remove failure details from collected data
   */
  def removeDetails(fq: FreqMap[Set[Any]]): FreqMap[Set[Any]] =
    fq.getCounts.foldLeft(FreqMap.empty[Set[Any]]) { case (res, (set, count)) =>
      set.toList match
        case h :: _ =>
          h.asInstanceOf[Matchable] match
            case _:execute.FailureDetails => res
            case _ => (1 to count).foldLeft(res) { case (map, i) => map + set }
        case _ =>
          (1 to count).foldLeft(res) { case (map, i) => map + set }
    }
