package org.specs2
package specification
package gen

import execute.Result
import org.scalacheck._
import org.specs2.specification

/**
 * Start a Given/When/Then sequence returning a generator to generate values of type T
 */
abstract class Given[T](regex: String = "") extends specification.Given[Gen[T]](regex) {
  def extract(text: String): Gen[T]
}
/**
 * Create a context from a previous context by adding more generated data
 */
abstract class When[P, T](regex: String = "") extends specification.When[Gen[P], Gen[T]](regex) {
  def extract(pg: Gen[P], text: String): Gen[T] = for (p <- pg; t <- extract(p, text)) yield t
  def extract(p: P, text: String): Gen[T]
}
/**
 * Check the state of the system by taking an arbitrary value and using ScalaCheck properties to generate a Result
 */
abstract class Then[T](regex: String = "") extends specification.Then[Gen[T]](regex) {
  def extract(tg: Gen[T], text: String): Result = extract(text)(Arbitrary { tg })
  def extract(text: String)(implicit g: Arbitrary[T]): Result
}
