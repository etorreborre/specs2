package org.specs2
package reporter

import org.specs2.internal.scalaz._
import Generator._
import Scalaz._
import main.Arguments
import specification._

/**
 * This trait defines a very generic way to store the results of an executed specification
 */
private[specs2]
trait Storing {

  /** @return a function storing ExecutedFragments */
  def store(implicit args: Arguments): Seq[ExecutedFragment] => Seq[ExecutedFragment]
}

private[specs2]
trait DefaultStoring extends Storing with Statistics {
  def store(implicit args: Arguments) = (fragments: Seq[ExecutedFragment]) => 
    (fragments zip FoldrGenerator[Seq].reduce(StatisticsReducer, fragments).totals).collect { 
      case (ExecutedSpecStart(n, a, l), s) => new ExecutedSpecStart(n, a, l) { override def stats = s }
      case (ExecutedSpecEnd(n, l), s)      => new ExecutedSpecEnd(n, l) { override def stats = s }
      case (other, s)                      => other
    }
  
}
