package org.specs2
package control

import io.ConsoleOutput
import scala.implicits.Not

/**
 * This trait provides simple a way to print out any object to the console:
 *
 *  "this string".pp must_== "this string"
 *
 *  will print 'this string' and pass it to the rest of the expectation
 *
 */
trait Debug extends ImplicitParameters:

  given DebugConsoleOutput as ConsoleOutput = ConsoleOutput

  extension [T](t: =>T)(using not: Not[NoDebug], output: ConsoleOutput)
    /** print the object to the console and return it */
    def pp: T =
      lazy val value = t
      output.println(value)
      value

    /** print the object to the console and return it, if the condition is satisfied */
    def pp(condition: Boolean): T =
      pp((t: T) => condition)

    /** print the object to the console and return it, if the condition is satisfied */
    def pp(condition: T => Boolean): T =
      lazy val value = t
      if condition(value) then value.pp else value

    /** print the object to the console with a specific function and return it */
    def pp(show: T => String)(using p: ImplicitParam): T =
      lazy val value = t
      Use(p)
      output.println(show(value))
      value

    /** print the object to the console with a small message before */
    def pp(pre: String): T =
      lazy val value = t
      output.println(pre+" "+value)
      value

/**
 * Use this trait to disable the `pp` method on objects
 */
trait NoDebug extends Debug:
  given NoDebug = ???

object Debug extends Debug
