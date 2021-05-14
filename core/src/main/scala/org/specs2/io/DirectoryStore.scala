package org.specs2
package io

import control.*
import specification.process.*
import fp.*, syntax.*

case class DirectoryStore(baseDirectory: DirectoryPath, fileSystem: FileSystem) extends Store:

  def set[A](key: Key[A], fact: A): Operation[Unit] =
    fileSystem.writeFile(filepath(key), StoreKeys.encode(key, fact))

  def get[A](key: Key[A]): Operation[Option[A]] =
    fileSystem.exists(filepath(key)).flatMap { e =>
      if e then fileSystem.readFile(filepath(key)).map(content => StoreKeys.decode(key, content))
      else   Operation.ok(None)
    }

  def reset: Operation[Unit] =
    fileSystem.delete(baseDirectory)

  private def filepath[A](key: Key[A]): FilePath =
    baseDirectory / FilePath.unsafe(StoreKeys.resolve(key))
