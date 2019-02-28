package org.specs2
package io

import control._
import FileSystem._
import specification.process._

case class DirectoryStore(baseDirectory: DirectoryPath) extends Store {

  def set[A](key: Key[A], fact: A): Operation[Unit] =
    writeFile(filepath(key), StoreKeys.encode(key, fact))

  def get[A](key: Key[A]): Operation[Option[A]] =
    exists(filepath(key)).flatMap { e =>
      if (e) readFile(filepath(key)).map(content => StoreKeys.decode(key, content))
      else   Operations.ok(None)
    }

  def reset: Operation[Unit] = delete(baseDirectory)

  private def filepath[A](key: Key[A]): FilePath =
    baseDirectory / FilePath.unsafe(StoreKeys.resolve(key))

}
