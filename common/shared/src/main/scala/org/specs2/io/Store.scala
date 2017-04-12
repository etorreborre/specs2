package org.specs2
package io

import control._

/**
 * Key-value store
 */
trait Store {
  def get[A](key: Key[A]): Operation[Option[A]]
  def set[A](key: Key[A], a: A): Operation[Unit]
  def reset: Operation[Unit]
}

trait Key[A]

