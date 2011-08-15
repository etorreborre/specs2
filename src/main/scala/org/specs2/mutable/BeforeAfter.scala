package org.specs2
package mutable

import org.specs2.execute.Result

trait BeforeAfter extends org.specs2.specification.BeforeAfter with DelayedInit {
  override def delayedInit(x: => Unit): Unit = try { before; x } finally { after }
}
