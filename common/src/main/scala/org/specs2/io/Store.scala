package org.specs2
package io

import control._

/**
 * Key-value store
 */
trait Store {
  def get[A](key: Key[A]): Action[Option[A]]
  def set[A](key: Key[A], a: A): Action[Unit]
  def reset: Action[Unit]
}

trait Key[A]

