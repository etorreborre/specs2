package org.specs2
package control

/**
 * This class simply encapsulates a lazy value which will be only evaluated once
 * @see org.specs2.specification.process.RandomSequentialExecution for an example of use
 */
case class LazyValue[T](t: () => T) {
  lazy val value = {
    t()
  }
}

