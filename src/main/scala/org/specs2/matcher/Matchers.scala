package org.specs2
package matcher
import execute._

trait Matchers extends AnyMatchers with IterableMatchers with StringMatchers with BeHaveMatchers with
      ExceptionMatchers
      
object Matchers extends Matchers