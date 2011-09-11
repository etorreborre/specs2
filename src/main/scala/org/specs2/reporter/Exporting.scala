package org.specs2
package reporter

import collection.Iterablex._
import main.Arguments
import specification._

/**
 * This trait defines a very generic way to export the result of an executed specification
 */
private[specs2]
trait Exporting {

  type ExportType

  /** @return a function exporting an ExecutedSpecification */
  def export(implicit args: Arguments): ExecutedSpecification => ExportType
}

/**
 * an executed specification with a name and a sequence of executed fragments
 */
case class ExecutedSpecification(name: SpecName, fragments: Seq[ExecutedFragment])

/**
 * for testing only
 */
private[specs2]
object ExecutedSpecification {
  def apply(fs: Seq[ExecutedFragment]): ExecutedSpecification = {
    fs match {
      case (s @ ExecutedSpecStart(_,_,_)) +: rest => ExecutedSpecification(s.specName, fs)
      case other                                  => ExecutedSpecification(SpecName(""), fs)
    }
  }
}
