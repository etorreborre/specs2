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
  
  def foldMap[F1](implicit args: Arguments, fmap: F1 => F): Function2[T, F1, T] = {
    (t: T, f1: F1) => fold(args)(t, fmap(f1))
  } 
  def fold(implicit args: Arguments): Function2[T, F, T]
  
  def foldMapAll[F1](fs: =>Seq[F1])(implicit args: Arguments, fmap: F1 => F): T = 
    foldAll(fs.view.map(fmap))(args)
    
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





