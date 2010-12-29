package org.specs2
package io

/**
 * The MockWriter writes all the content written with the Writer interface to a list of messages
 */
private[specs2]
trait MockWriter extends java.io.Writer with MockOutput {
  /** is the Writer closed? */
  var closed = false
  override def write(m: String) : Unit = print(m)

  /** closes the Writer */
  override def close = closed = true

  /** flushes the Writer */
  override def flush = {}

  /** overrides the write(a: Array[Char], b: Int, c: Int) method to do nothing */
  override def write(a: Array[Char], b: Int, c: Int) = {}
}
