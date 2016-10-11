package org.specs2
package specification
package process

import text.Regexes._
import control._
import producer._
import data._
import scalaz._, Scalaz._
import specification.core._
import Fragment._
import org.specs2.specification.create.FormattingFragments

/**
 * Selection function for Fragment processes
 */
trait Selector {

  /** select fragments by name, markers and previous execution */
  def select(env: Env): Transducer[ActionStack, Fragment, Fragment]

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
  def select(env: Env): Transducer[ActionStack, Fragment, Fragment] =
    filterByName(env: Env) |> filterByMarker(env) |> filterByPrevious(env)

  /** filter fragments by name */
  def filterByName(env: Env): Transducer[ActionStack, Fragment, Fragment] = {
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
  def filterByMarker(env: Env): Transducer[ActionStack, Fragment, Fragment] = {
    val arguments = env.arguments

    def go(sections: List[NamedTag] = Nil): Transducer[ActionStack, Fragment, Fragment] = {
      receive1 {
        case Fragment(m @ Marker(t, _, _),_,_)  =>
          go(updateSections(sections, t))

        case fragment if !Fragment.isFormatting(fragment) && !Fragment.isEmptyText(fragment) =>
          val apply = sections.sumr
          val keep = apply.keep(arguments, apply.names)
          emit(fragment).filter(_ => keep) fby go(sections)

        case fragment =>
          emit(fragment) fby go(sections)
      }
    }

    if ((arguments.include + arguments.exclude).nonEmpty) normalize |> go() |> removeAdditionalEmptyText
    else transducers.id
  }

  def normalize: Transducer[ActionStack, Fragment, Fragment] =
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
  def transformBeforeMarkersToAfterMarkers: Transducer[ActionStack, Fragment, Fragment] = {
    def go(previous: Option[Fragment] = None, sections: List[NamedTag] = Nil): Process1[(Fragment, Option[Fragment]), Fragment] = {
      receive1 {
        // section for before if this is the start of a section, do a swap
        case (f @ Fragment(m @ Marker(t, true, false),_,_), _) if !isEndTag(sections, t) =>
          emitAll(f.copy(description = m.copy(appliesToNext = true)) +: previous.toSeq) fby go(sections = updateSections(sections, t))

        // section for before if this is the end of a section, don't swap
        case (m @ Fragment(Marker(t, true, false),_,_), _) if isEndTag(sections, t) =>
          emitAll(previous.toSeq :+ m) fby go(sections = updateSections(sections, t))

        // tag for before
        case (f @ Fragment(m @ Marker(t, false, false),_,_), _)  =>
          emitAll(f.copy(description = m.copy(appliesToNext = true)) +: previous.toSeq) fby go(None, sections)

        case (f, Some(_)) =>
          emitAll(previous.toSeq) fby go(Some(f), sections)

        case (f, None) =>
          emitAll(previous.toSeq :+ f)
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
    sections.filter(_.names.exists(tag.names.contains)).nonEmpty

  def swapBeforeMarkerAndEmptyText: Transducer[ActionStack, Fragment, Fragment] = {
    def go(previous: Option[Fragment] = None): Transducer[ActionStack, (Fragment, Option[Fragment]), Fragment] = {
      receive1 {
        // tag or section for before
        case (m @ Fragment(Marker(t, _, false),_,_), _)  =>
          if (previous.exists(isEmptyText))
            emitAll(m +: previous.toSeq) fby go()
          else
            emitAll(previous.toSeq :+ m) fby go()

        case (f, Some(_)) =>
          emitAll(previous.toSeq) fby go(Some(f))

        case (f, None) =>
          emitAll(previous.toSeq :+ f)
      }
    }
    zipWithNext |> go()
  }

  def swapAfterMarkerAndEmptyText: Transducer[ActionStack, Fragment, Fragment] = {
    def go(previous: Option[Fragment] = None): Transducer[ActionStack, (Fragment, Option[Fragment]), Fragment] = {
      receive1 {
        // tag or section for after
        case (m @ Fragment(Marker(t, _, true),_,_), _) =>
         emitAll(previous.toSeq) fby go(Some(m))

        case (f, Some(_)) if isEmptyText(f) =>
          emit(f) fby go(previous)

        case (f, Some(_)) =>
          emitAll(previous.toSeq :+ f) fby go()

        case (f, None) =>
          emitAll(previous.toSeq :+ f)
      }
    }
    zipWithNext |> go()
  }

  def transformTagsToSections: Transducer[ActionStack, Fragment, Fragment] = {
    def go(previous: List[Fragment] = Nil): Transducer[ActionStack, Fragment, Fragment] = {
      receive1 {
        case f @ Fragment(m @ Marker(t, false, _),_,_) =>
          go(previous :+ f.copy(description = m.copy(isSection = true)))

        case f =>
          emitAll(previous ++ Seq(f) ++ previous) fby go()
      }
    }
    go()
  }

  def removeAdditionalEmptyText: Transducer[ActionStack, Fragment, Fragment] = {
    def go: Transducer[ActionStack, Fragment, Fragment] = {
      receive1 { f  =>
        if (Fragment.isEmptyText(f) || Fragment.isFormatting(f))
          go
        else
          emit(FormattingFragments.br) fby emit(f) fby go
      }
    }
    emit(FormattingFragments.br) fby go
  }

  /**
   * filter fragments by previous execution and required status
   */
  def filterByPrevious(env: Env): Transducer[ActionStack, Fragment, Fragment] =
    if (env.arguments.wasIsDefined) transducers.filter((_: Fragment).was(env.arguments.was))
    else transducers.id

}

object DefaultSelector extends DefaultSelector
