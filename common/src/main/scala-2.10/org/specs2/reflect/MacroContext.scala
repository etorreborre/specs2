package org.specs2
package reflect

trait MacroContext {
  type Context = scala.reflect.macros.Context
}
object MacroContext extends MacroContext