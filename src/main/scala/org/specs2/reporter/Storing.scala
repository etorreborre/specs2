package org.specs2
package reporter

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
trait DefaultStoring extends Storing {
  def store(implicit args: Arguments) = (fragments: Seq[ExecutedFragment]) => fragments
}
