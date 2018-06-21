package org.specs2.matcher

import cats.data.NonEmptyList
import org.specs2.data.Sized

trait NonEmptyListMatchers {

  implicit def nonEmptyListIsSized[T]: Sized[NonEmptyList[T]] = new Sized[NonEmptyList[T]] {
    override def size(t: NonEmptyList[T]): Int = t.length
  }
}

object NonEmptyListMatchers {
}
