package org.specs2
package specification
package core

import org.specs2.fp._
import Fragment._
import control._
import producer._
import Producer._
import org.specs2.concurrent.ExecutionEnv
import ExecuteActions._

/**
 * Fragments of a specification
 *
 * It is implemented as a Process of Fragment in order to produce fragments
 * dynamically if necessary
 */
case class Fragments(contents: AsyncStream[Fragment]) {
  /** append one or several fragments to this process */

  def append(other: Fragment): Fragments       = append(emitAsync(other))
  def append(others: Seq[Fragment]): Fragments = append(Fragments(others:_*))
  def append(others: Fragments): Fragments     = append(others.contents)
  def appendLazy(other: =>Fragment): Fragments = append(emitAsyncDelayed(other))

  /** prepend one or several fragments to this process */

  def prepend(other: Fragment): Fragments       = prepend(emitAsync(other))
  def prepend(others: Fragments): Fragments     = prepend(others.contents)
  def prepend(others: Seq[Fragment]): Fragments = prepend(Fragments(others:_*))
  def prependLazy(other: =>Fragment): Fragments = prepend(emitAsyncDelayed(other))

  /** filter, map or flatMap the fragments */

  def when(condition: =>Boolean) = {
    lazy val c = condition
    copy(contents = contents filter (_ => c))
  }

  def map(f: Fragment => Fragment): Fragments =
    copy(contents = contents map f)

  def mapFragments(f: List[Fragment] => List[Fragment]): Fragments =
    copy(contents = Producer.emitEff(contents.runList.map(f)))

  def mapDescription(f: Description => Description): Fragments =
    map(_.updateDescription(f))

  def filter(predicate: Fragment => Boolean): Fragments =
    copy(contents = contents filter predicate)

  def collect[A](predicate: PartialFunction[Fragment, A]): AsyncStream[A] =
    contents collect predicate

  def update(f: AsyncTransducer[Fragment, Fragment])   = copy(contents = f(contents))
  def flatMap(f: Fragment => AsyncStream[Fragment])    = copy(contents = contents flatMap f)
  def |> (f: AsyncTransducer[Fragment, Fragment])  = copy(contents = f(contents))
  def append(other: AsyncStream[Fragment]): Fragments  = copy(contents = contents append other)
  def prepend(other: AsyncStream[Fragment]): Fragments = copy(contents = other append contents)
  def updateFragments(update: List[Fragment] => Fragments): Fragments =
    copy(Producer.emitEff(contents.runList.flatMap(fs => update(fs).contents.runList)))

  /** run the process to get all fragments */
  def fragments: Action[List[Fragment]] =
    ProducerOps(contents).runList

  /** run the process to get all fragments as a list */
  def fragmentsList(ee: ExecutionEnv): List[Fragment] =
    ProducerOps(contents).runList.run(ee)

  /** run the process to filter all texts */
  def texts = filter(isText).fragments

  /** run the process to filter all markers */
  def markers = filter(isMarker).fragments

  /** run the process to filter all examples */
  def examples = filter(isExample).fragments

  /** run the process to collect all tags */
  def tags = collect(marker).map(_.tag)

  /** run the process to get all specification references as Fragments */
  def referenced = filter(isSpecificationRef).fragments

  /** run the process to get all specification references */
  def specificationRefs: AsyncStream[SpecificationRef] =
    collect(specificationRef)

  /** run the process to get all specification see references */
  def seeReferences: AsyncStream[SpecificationRef] =
    collect(seeReference)

  /** run the process to get all specification link references */
  def linkReferences: AsyncStream[SpecificationRef] =
    collect(linkReference)

  /** strip the margin of all examples */
  def stripMargin: Fragments = stripMargin('|')

  /** strip the margin of all examples */
  def stripMargin(margin: Char): Fragments = mapDescription(_.stripMargin(margin))

  /** when 2 Text fragments are contiguous append them together to only make one */
  def compact = Fragments {
    type S = Option[String]

    contents.producerState[Fragment, S](None, Some(s => s.fold(done[ActionStack, Fragment])(t => one(Fragment(Text(t)))))) {
      case (f, text) =>
        f match {
          case Fragment(Text(t),l, e) if isText(f) =>
            (done, text.map(_+t).orElse(Some(t)))

          case other =>
            text match {
              case Some(t1) => (Producer.emit(List(Fragment(Text(t1)), other)), None)
              case None     => (one(other), None)
            }
        }
    }
  }

}

object Fragments {

  /** empty sequence of fragments */
  val empty = Fragments()

  /** create fragments from a sequence of individual fragments */
  def apply(fragments: Fragment*): Fragments =
    new Fragments(emitAsync(fragments:_*))

  implicit def FragmentsMonoid: Monoid[Fragments] = new Monoid[Fragments] {
    def zero : Fragments = Fragments.empty

    def append(fs1: Fragments, fs2: =>Fragments): Fragments =
      fs1.append(fs2)
  }

  /** iterate over elements to create a Fragments object */
  def foreach[T](seq: Seq[T])(f: T => Fragments): Fragments =
    seq.foldLeft(Fragments.empty)((res, cur) => res.append(f(cur)))
}

