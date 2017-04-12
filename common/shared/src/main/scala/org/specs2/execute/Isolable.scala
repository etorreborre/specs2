package org.specs2
package execute

/**
 * This trait models elements which can be executed in a brand new context with new local variables
 * It is used to execute examples in a new specification when needed
 */
trait Isolable {
  def isolable: Boolean
}
