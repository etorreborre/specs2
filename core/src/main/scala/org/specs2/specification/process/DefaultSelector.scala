package org.specs2
package specification
package process

import text.Regexes._
import control._
import producer._, transducers._, producers._
import data._
import scalaz._, Scalaz._
import specification.core._
import create.FormattingFragments
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
      }

    if ((arguments.include + arguments.exclude).nonEmpty) normalize |> go.filter(_.isDefined) |> removeAdditionalEmptyText
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
    def go(previous: Option[Fragment] = None, sections: List[NamedTag] = Nil): AsyncTransducer[(Fragment, Option[Fragment]), Fragment] = {
      receive {
        // section for before if this is the start of a section, do a swap
        case (f @ Fragment(m @ Marker(t, true, false),_,_), _) if !isEndTag(sections, t) =>
          emit(f.copy(description = m.copy(appliesToNext = true)) +: previous.toList) append go(sections = updateSections(sections, t))

        // section for before if this is the end of a section, don't swap
        case (m @ Fragment(Marker(t, true, false),_,_), _) if isEndTag(sections, t) =>
          emit(previous.toList :+ m) append go(sections = updateSections(sections, t))

        // tag for before
        case (f @ Fragment(m @ Marker(t, false, false),_,_), _)  =>
          emit(f.copy(description = m.copy(appliesToNext = true)) +: previous.toList) append go(None, sections)

        case (f, Some(_)) =>
          emit(previous.toList) append go(Some(f), sections)

        case (f, None) =>
          emit(previous.toList :+ f)
      }
    }
    zipWithNext |> go()
  }

  def updateSections(sections: List[NamedTag], tag: NamedTag): List[NamedTag] = {
    val endTags     = sections.filter(_.names.exists(tag.names.contains))
    val startTags   = sections.map(t => t.removeNames(tag.names)).filterNot(_.names.isEmpty)
    if (endTags.isEmpty) startTags :+ tag else startTags
  }

  def isEndTag(sections: List[NamedTag], tag: NamedTag): Boolean =
    sections.exists(_.names.exists(tag.names.contains))

  def swapBeforeMarkerAndEmptyText: AsyncTransducer[Fragment, Fragment] = {
    def go(previous: Option[Fragment] = None): AsyncTransducer[(Fragment, Option[Fragment]), Fragment] = {
      receive {
        // tag or section for before
        case (m @ Fragment(Marker(t, _, false),_,_), _)  =>
          if (previous.exists(isEmptyText))
            emit(m +: previous.toList) append go()
          else
            emit(previous.toList :+ m) append go()

        case (f, Some(_)) =>
          emit(previous.toList) append go(Some(f))

        case (f, None) =>
          emit(previous.toList :+ f)
      }
    }
    zipWithNext |> go()
  }

  def swapAfterMarkerAndEmptyText: AsyncTransducer[Fragment, Fragment] = {
    def go(previous: Option[Fragment] = None): AsyncTransducer[(Fragment, Option[Fragment]), Fragment] = {
      receive {
        // tag or section for after
        case (m @ Fragment(Marker(t, _, true),_,_), _) =>
         emit(previous.toList) append go(Some(m))

        case (f, Some(_)) if isEmptyText(f) =>
          emit(f) append go(previous)

        case (f, Some(_)) =>
          emit(previous.toList :+ f) append go()

        case (f, None) =>
          emit(previous.toList :+ f)
      }
    }
    zipWithNext |> go()
  }

  def transformTagsToSections: AsyncTransducer[Fragment, Fragment] = {
    def go(previous: List[Fragment] = Nil): AsyncTransducer[Fragment, Fragment] = {
      receive {
        case f @ Fragment(m @ Marker(t, false, _),_,_) =>
          go(previous :+ f.copy(description = m.copy(isSection = true)))

        case f =>
          emit(previous ++ Seq(f) ++ previous) append go()
      }
    }
    go()
  }

  def removeAdditionalEmptyText: AsyncTransducer[Fragment, Fragment] = {
    def go: AsyncTransducer[Fragment, Fragment] = {
      receive { f  =>
        if (Fragment.isEmptyText(f) || Fragment.isFormatting(f))
          go
        else
          emit(FormattingFragments.br) append emit(f) append go
      }
    }
    emit(FormattingFragments.br) append go
  }

  /**
   * filter fragments by previous execution and required status
   */
  def filterByPrevious(env: Env): AsyncTransducer[Fragment, Fragment] =
    if (env.arguments.wasIsDefined) transducers.filter((_: Fragment).was(env.arguments.was))
    else transducers.id

}

object DefaultSelector extends DefaultSelector
