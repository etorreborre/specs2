package org.specs2
package specification
package core

import scalaz.Monoid
import scalaz.concurrent.Task
import org.specs2.codata._
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

  def when(condition: =>Boolean) = copy(contents = contents when emit(condition))

  def map(f: Fragment => Fragment)                                  = copy(contents = contents map f)
  def mapDescription(f: Description => Description)                 = map(_.updateDescription(f))
  def filter(predicate: Fragment => Boolean)                        = copy(contents = contents filter predicate)

  def update(f: Process[Task, Fragment] => Process[Task, Fragment]) = copy(contents = f(contents))
  def flatMap(f: Fragment => Process[Task, Fragment])               = copy(contents = contents flatMap f)
  def |> (other: Process1[Fragment, Fragment])                      = copy(contents = contents |> other)
  def append(other: Process[Task, Fragment]): Fragments             = copy(contents = contents fby other)
  def prepend(other: Process[Task, Fragment]): Fragments            = copy(contents = other fby contents)

  /** run the process to get all fragments */
  def fragments: IndexedSeq[Fragment] = contents.runLog.run

  /** run the process to filter all texts */
  def texts = fragments.filter(isText)

  /** run the process to filter all markers */
  def markers = fragments.filter(isMarker)

  /** run the process to filter all examples */
  def examples = fragments.filter(isExample)

  /** run the process to collect all tags */
  def tags = fragments.collect(marker).map(_.tag)

  /** run the process to get all specification references as Fragments */
  def referenced = fragments.filter(isSpecificationRef)

  /** run the process to get all specification references */
  def specificationRefs = fragments.collect(specificationRef)

  /** run the process to get all specification see references */
  def seeReferences = fragments.collect(seeReference)

  /** run the process to get all specification link references */
  def linkReferences = fragments.collect(linkReference)


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
  /** empty sequence of fragments */
  val empty = Fragments()

  /** create fragments from a sequence of individual fragments */
  def apply(fragments: Fragment*): Fragments =
    Fragments(emitAll(fragments).toSource)

  implicit def FragmentsMonoid: Monoid[Fragments] = new Monoid[Fragments] {
    def zero : Fragments = Fragments.empty

    def append(fs1: Fragments, fs2: =>Fragments): Fragments =
      fs1.append(fs2)
  }

  /** iterate over elements to create a Fragments object */
  def foreach[T](seq: Seq[T])(f: T => Fragments): Fragments =
    seq.foldLeft(Fragments.empty)((res, cur) => res.append(f(cur)))
}

