package org.specs2
package reflect

/**
 * to remove 2.11 warnings
 */
object MacroContext {
  object blackbox {
    type Context = scala.reflect.macros.blackbox.Context
  }
}
