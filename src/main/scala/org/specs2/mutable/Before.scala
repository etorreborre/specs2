package org.specs2
package mutable

/**
 * This trait adds the possibility to execute the before behavior before the body of the context.
 *
 * Since the delayedInit method doesn't return a Result, this only works with mutable specifications where results are
 * thrown as exceptions
 */
trait Before extends org.specs2.specification.Before with DelayedInit {
  override def delayedInit(x: => Unit): Unit = { before; x }
}
