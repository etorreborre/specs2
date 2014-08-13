package org.specs2
package reflect

/**
 * to remove 2.11 warnings
 */
trait MacroContext {
  type Context = scala.reflect.macros.blackbox.Context
}
object MacroContext extends MacroContext
