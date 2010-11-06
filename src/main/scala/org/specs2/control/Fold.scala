package org.specs2
package control

import Exceptions._
import Throwablex._
import main.Arguments
/**
 * A Fold transforms a list of elements F to a given type T, starting from an initial value.
 * 
 * It is either used to:
 * * map each F to something else 
 * * to accumulate values of type T
 * 
 */
trait Fold[F] {
  type T
  def initial: T
  
  def fold(implicit args: Arguments): Function2[T, F, T]
  
  def foldAll(fs: =>Seq[F])(implicit args: Arguments): T = trye {
    fs.foldLeft(initial)(fold(args))
  } ((e: Exception) => handleException(e)) match {
    case Right(e) => e
    case Left(e) => initial
  }
  def handleException(e: Exception) = {
    Console.println("There was an exception during the building of the sequence to fold: " + e)
    e.printFullStackTrace
  }
}





