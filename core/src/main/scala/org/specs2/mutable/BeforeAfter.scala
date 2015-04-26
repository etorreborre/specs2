package org.specs2
package mutable

import matcher.Scope

trait BeforeAfter extends org.specs2.specification.BeforeAfter with DelayedInit with Scope {
  override def delayedInit(x: => Unit): Unit = try { before; x } finally { after }
}
