package org.specs2
package specification
package process

import text.Regexes._
import control._
import producer._, transducers._, producers._
import data._
import scalaz._, Scalaz.{ToIdOps => _,_}
import specification.core._
import Fragment._

/**
 * Selection function for Fragment processes
 */
trait Selector {

  /** select fragments by name, markers and previous execution */
  def select(env: Env): AsyncTransducer[Fragment, Fragment]

}

/**
 * Default selection for specification fragments:
 * 
 *  - filter based on the name
 *  - filter based on the tags
 *  - filter based on previous execution
 */
trait DefaultSelector extends Selector {

  /** select fragments by name, markers and previous execution */
  def select(env: Env): AsyncTransducer[Fragment, Fragment] =
    filterByName(env: Env) |> filterByMarker(env) |> filterByPrevious(env)

  /** filter fragments by name */
  def filterByName(env: Env): AsyncTransducer[Fragment, Fragment] = {
    val regex = env.arguments.ex
    if (regex !=".*")
      transducers.filter {
        case Fragment(Text(t),e,_) if e.isExecutable => t matchesSafely regex
        case Fragment(Code(t),e,_) if e.isExecutable => t matchesSafely regex
        case other                                   => true
      }
    else transducers.id
  }

  /**
   * filter fragments by markers
   *
   * This method is a bit involved but we have to consider lots of different cases
   *
   * - if the marker is a tag or a section
   * - if the marker applies to the previous or next fragment
   * - if there is an irrelevant empty text between the marker and the fragment it applies to
   */
  def filterByMarker(env: Env): AsyncTransducer[Fragment, Fragment] = {
    val arguments = env.arguments

    def go: AsyncTransducer[Fragment, Fragment] =
      transducers.state[ActionStack, Fragment, Option[Fragment], List[NamedTag]](Nil) {
        case (f @ Fragment(m @ Marker(t, _, _),_,_), sections)  =>
          (Option(f), updateSections(sections, t))

        case (fragment, sections) if !Fragment.isFormatting(fragment) && !Fragment.isEmptyText(fragment) =>
          val apply = sections.sumr
          val keep = apply.keep(arguments, apply.names)
          (Option(fragment).filter(_ => keep), sections)

        case (fragment, sections) =>
          (Option(fragment), sections)
      } flatMap (f => emit(f.toList))

    if ((arguments.include + arguments.exclude).nonEmpty) normalize |> go |> removeMarkers
    else transducers.id
  }

  def normalize: AsyncTransducer[Fragment, Fragment] =
    swapBeforeMarkerAndEmptyText         |>
    swapAfterMarkerAndEmptyText          |>
    transformBeforeMarkersToAfterMarkers |>
    transformTagsToSections

  /**
   * All the "appliesToNext = false" markers must be transformed into "appliesToNext = true"
   * except when they are the end of a section.
   *
   * This is because we want to visually include all of e2, e3, e4 in the following acceptance spec
   *
   * e1
   * e2 \${section("x")}
   * e3
   * e4 \${section("x")}
   * e5
   */
  def transformBeforeMarkersToAfterMarkers: AsyncTransducer[Fragment, Fragment] = {
    def go: AsyncTransducer[(Fragment, Option[Fragment]), Fragment] = {
      type S = (Option[Fragment], List[NamedTag])

      transducers.producerState[ActionStack, (Fragment, Option[Fragment]), Fragment, S]((None, Nil)){
        // section for before if this is the start of a section, do a swap
        case ((f @ Fragment(m @ Marker(t, true, false),_,_), _), (previous, sections)) if !isEndTag(sections, t) =>
          (emit(f.copy(description = m.copy(appliesToNext = true)) +: previous.toList), (None, updateSections(sections, t)))

        // section for before if this is the end of a section, don't swap
        case ((m @ Fragment(Marker(t, true, false),_,_), _), (previous, sections)) if isEndTag(sections, t) =>
          (emit(previous.toList :+ m), (None, updateSections(sections, t)))

        // tag for before
        case ((f @ Fragment(m @ Marker(t, false, false),_,_), _), (previous, sections))  =>
          (emit(f.copy(description = m.copy(appliesToNext = true)) +: previous.toList), (None, sections))

        case ((f, Some(_)), (previous, sections)) =>
          (emit(previous.toList), (Option(f), sections))

        case ((f, None), (previous, sections)) =>
          (emit(previous.toList :+ f), (None, Nil))
      }
    }
    zipWithNext[ActionStack, Fragment] |> go
  }

  def updateSections(sections: List[NamedTag], tag: NamedTag): List[NamedTag] = {
    val endTags     = sections.filter(_.names.exists(tag.names.contains))
    val startTags   = sections.map(t => t.removeNames(tag.names)).filterNot(_.names.isEmpty)
    if (endTags.isEmpty) startTags :+ tag else startTags
  }

  def isEndTag(sections: List[NamedTag], tag: NamedTag): Boolean =
    sections.exists(_.names.exists(tag.names.contains))

  def swapBeforeMarkerAndEmptyText: AsyncTransducer[Fragment, Fragment] = {
    def go: AsyncTransducer[(Fragment, Option[Fragment]), Fragment] = {
      transducers.producerState[ActionStack, (Fragment, Option[Fragment]), Fragment, Option[Fragment]](None) {
        // tag or section for before
        case ((m @ Fragment(Marker(t, _, false),_,_), _), previous)  =>
          if (previous.exists(isEmptyText))
            (emit(m +: previous.toList), None)
          else
            (emit(previous.toList :+ m), None)

        case ((f, Some(_)), previous) =>
          (emit(previous.toList), Some(f))

        case ((f, None), previous) =>
          (emit(previous.toList :+ f), None)
      }
    }
    zipWithNext[ActionStack, Fragment] |> go
  }

  def swapAfterMarkerAndEmptyText: AsyncTransducer[Fragment, Fragment] = {
    def go: AsyncTransducer[(Fragment, Option[Fragment]), Fragment] = {
      transducers.producerState[ActionStack, (Fragment, Option[Fragment]), Fragment, Option[Fragment]](None) {
        // tag or section for after
        case ((m @ Fragment(Marker(t, _, true),_,_), _), previous) =>
          (emit(previous.toList), Some(m))

        case ((f, Some(_)), previous) if isEmptyText(f) =>
          (one(f), previous)

        case ((f, Some(_)), previous) =>
          (emit(previous.toList :+ f), None)

        case ((f, None), previous) =>
          (emit(previous.toList :+ f), None)
      }
    }
    zipWithNext[ActionStack, Fragment] |> go
  }

  def transformTagsToSections: AsyncTransducer[Fragment, Fragment] = {
    transducers.producerState[ActionStack, Fragment, Fragment, List[Fragment]](Nil) {
      case (f @ Fragment(m @ Marker(t, false, _),_,_), previous) =>
        (done, previous :+ f.copy(description = m.copy(isSection = true)))

      case (f, previous) =>
        (emit(previous ++ List(f) ++ previous), Nil)
    }
  }

  def removeMarkers: AsyncTransducer[Fragment, Fragment] = { producer: Producer[ActionStack, Fragment] =>
    producer.filter(f => !Fragment.isMarker(f))
  }

  /**
   * filter fragments by previous execution and required status
   */
  def filterByPrevious(env: Env): AsyncTransducer[Fragment, Fragment] =
    if (env.arguments.wasIsDefined) transducers.filter((_: Fragment).was(env.arguments.was))
    else transducers.id

}

object DefaultSelector extends DefaultSelector
