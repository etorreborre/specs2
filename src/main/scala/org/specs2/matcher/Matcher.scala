package org.specs2
package matcher
import specification._
import execute._

trait Matcher[-T] {
  def apply(t: =>T): Result 
}