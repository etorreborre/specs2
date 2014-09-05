package org.specs2
package specification
package core

import scalaz.concurrent.Task
import execute.{AsResult, Result}
import shapeless._
import Fragments._
import scalaz.stream._
import Process._
import Fragment._

/**
 * Fragments of a specification
 *
 * It is implemented as a Process of Fragment in order to produce fragments
 * dynamically if necessary
 */
case class Fragments(contents: Process[Task, Fragment]) {
  /** append one or several fragments to this process */

  def append(other: Fragment): Fragments                = append(Process(other))
  def append(others: Seq[Fragment]): Fragments          = append(Fragments(others:_*))
  def append(others: Fragments): Fragments              = append(others.contents)
  def appendLazy(other: =>Fragment): Fragments          = append(Process.eval(Task.delay(other)))

  /** prepend one or several fragments to this process */

  def prepend(other: Fragment): Fragments                = prepend(Process(other))
  def prepend(others: Fragments): Fragments              = prepend(others.contents)
  def prepend(others: Seq[Fragment]): Fragments          = prepend(Fragments(others:_*))
  def prependLazy(other: =>Fragment): Fragments          = prepend(Process.eval(Task.delay(other)))

  /** filter, map or flatMap the fragments */

  def when(condition: =>Boolean) = contentsLens.modify(this)(_  when emit(condition))

  def map(f: Fragment => Fragment)                                  = contentsLens.modify(this)(_ map f)
  def mapDescription(f: Description => Description)                 = map(_.updateDescription(f))
  def filter(predicate: Fragment => Boolean)                        = contentsLens.modify(this)(_ filter predicate)

  def update(f: Process[Task, Fragment] => Process[Task, Fragment]) = contentsLens.modify(this)(f)
  def flatMap(f: Fragment => Process[Task, Fragment])               = contentsLens.modify(this)(_ flatMap f)
  def |> (other: Process1[Fragment, Fragment])                      = contentsLens.modify(this)(_ |> other)
  def append(other: Process[Task, Fragment]): Fragments             = contentsLens.modify(this)(_ fby other)
  def prepend(other: Process[Task, Fragment]): Fragments            = contentsLens.modify(this)(other fby _)

  /** run the process to get all fragments */
  def fragments: IndexedSeq[Fragment] = contents.runLog.run

  /** run the process to get all texts */
  def texts                           = fragments.filter(isText)

  /** run the process to get all examples */
  def examples                        = fragments.filter(isExample)

  /** strip the margin of all examples */
  def stripMargin: Fragments = stripMargin('|')

  /** strip the margin of all examples */
  def stripMargin(margin: Char): Fragments = mapDescription(_.stripMargin(margin))

  /** when 2 Text fragments are contiguous append them together to only make one */
  def compact = {
    val (text1, accumulated1) = fragments.foldLeft((None, Vector()): (Option[String], Seq[Fragment])) { case ((text, accumulated), fragment) =>
      fragment match {
        case Fragment(Text(t),l, e) if isText(fragment) =>
          (text.map(_+t).orElse(Some(t)), accumulated)

        case other =>
          text match {
            case Some(t1) => (None, accumulated ++ Seq(Fragment(Text(t1), Execution.NoExecution), other))
            case None     => (None, accumulated :+ other)
          }
      }
    }
    val compacted = text1.fold(accumulated1)(t => accumulated1 :+ Fragment(Text(t), Execution.NoExecution))
    Fragments(compacted:_*)
  }

}

object Fragments {
  val contentsLens = lens[Fragments] >> 'contents

  def apply(fragments: Fragment*): Fragments = Fragments(emitAll(fragments).toSource)
}


object Results {
  /** this allows the creation of expectations with a for loop */
  def foreach[T, R : AsResult](seq: Seq[T])(f: T => R): Result = {
    seq foreach f
    org.specs2.execute.Success()
  }
}

