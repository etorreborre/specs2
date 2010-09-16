package org.specs2
package execute

/**
 * Trait for anything that can be executed to return a Result
 */
trait Executable {
  /** @return a Result */	
  def execute: Result
}
