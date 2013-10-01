package org.specs2
package mutable

/**
 * This trait adds the possibility to execute the after behavior after the body of the context.
 * 
 * Since the delayedInit method doesn't return a Result, this only works with mutable specifications where results are
 * thrown as exceptions 
 */
trait After extends org.specs2.specification.After with DelayedInit {
  override def delayedInit(x: => Unit): Unit = try x finally after
}