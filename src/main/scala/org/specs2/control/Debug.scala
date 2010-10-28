package org.specs2
package control

private[specs2] trait Debug {
  implicit def printAndPass[T](t: =>T) = new { 
    def pp: T = { Console.println(t); t } 
  } 
}
object Debug extends Debug