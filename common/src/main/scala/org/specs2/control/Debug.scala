package org.specs2
package control

import io.ConsoleOutput

/** 
 * This trait provides simple a way to print out any object to the console:
 * 
 *  "this string".pp must_== "this string"
 *  
 *  will print 'this string' and pass it to the rest of the expectation
 *
 */
trait Debug extends ImplicitParameters {
  
  implicit def debug[T](t: =>T): Debuggable[T] = new Debuggable(t)
  class Debuggable[T](t: =>T) extends ConsoleOutput {
    lazy val value = t

    /** print the object to the console and return it */
    def pp: T = { println(value); value }

    /** print the object to the console and return it, if the condition is satisfied */
    def pp(condition: Boolean): T = pp((t: T) => condition)

    /** print the object to the console and return it, if the condition is satisfied */
    def pp(condition: T => Boolean): T = if (condition(value)) pp else value
    /** print the object to the console with a specific function and return it */
    def pp(show: T => String)(implicit p: ImplicitParam): T = { Use(p); println(show(value)); value }
    /** print the object to the console with a small message before */
    def pp(pre: String): T = { println(pre+" "+value); value }

  }

}

/**
 * Use this trait to disable the `pp` method on objects
 */
trait NoDebug extends Debug {
  override def debug[T](t: =>T) = super.debug(t)
}

object Debug extends Debug
