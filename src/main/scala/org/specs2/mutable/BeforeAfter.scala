package org.specs2
package mutable

trait BeforeAfter extends org.specs2.specification.BeforeAfter with DelayedInit {
  override def delayedInit(x: => Unit): Unit = try { before; x } finally { after }
}
