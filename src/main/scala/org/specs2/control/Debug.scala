package org.specs2
package control

/** 
 * This trait provides simple a way to print out any object to the console:
 * 
 *  "this string".pp must_== "this string"
 *  
 *  will print 'this string' and pass it to the rest of the expectation
 *
 */
private[specs2] 
trait Debug {
  
  implicit def debug[T](t: =>T): Debuggable[T] = new Debuggable(t)
  class Debuggable[T](t: =>T) {
    lazy val value = t
    /** print the object to the console and return it */
    def pp: T = { Console.println(value); value }
    /** print the object to the console and return it, if the condition is satisfied*/
    def pp(condition: Boolean): T = pp(v => condition)
    /** print the object to the console and return it, if the condition is satisfied*/
    def pp(condition: T => Boolean): T = if (condition(value)) pp else value
  }

}

/**
 * Use this trait to disable the `pp` method on objects
 */
trait NoDebug extends Debug {
  override def debug[T](t: =>T) = super.debug(t)
}

private[specs2] 
object Debug extends Debug