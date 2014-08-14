package org.specs2
package specification
package process

import scalaz.stream._
import scalaz.stream.Process.{Env =>_,_}
import data._
import scalaz.syntax.foldable._
import scalaz.std.list._
import data.Processes._
import specification.core._

/**
 * Selection function for Fragment processes
 */
trait Selector {

  /** select fragments by name, markers and previous execution */
  def select(env: Env): Process1[Fragment, Fragment]
}

trait DefaultSelector extends Selector {

  /** select fragments by name, markers and previous execution */
  def select(env: Env): Process1[Fragment, Fragment] =
    filterByName(env: Env) |> filterByMarker(env) |> filterByPrevious(env)

  /** filter fragments by name */
  def filterByName(env: Env): Process1[Fragment, Fragment] = {
    val regex = env.arguments.ex
    if (regex !=".*")
      process1.filter {
        case Fragment(Text(t),e,_) if e.isRunnable => t.matches(regex)
        case other                                    => true
      }
    else process1.id
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
  def filterByMarker(env: Env): Process1[Fragment, Fragment] = {
    val arguments = env.arguments

    def go(sections: List[NamedTag] = Nil): Process1[(Option[Fragment], Fragment, Option[Fragment]), Fragment] = {

      def updateSections(tag: NamedTag) = {
        val endTags     = sections.filter(_.names.exists(tag.names.contains))
        val startTags   = sections.map(t => t.removeNames(tag.names)).filterNot(_.names.isEmpty)
        if (endTags.isEmpty) startTags :+ tag else startTags
      }

      def filter(fragment: Fragment, tags: List[NamedTag] = sections, tag: NamedTag = AlwaysWhenNoIncludeTag): Process1[(Option[Fragment], Fragment, Option[Fragment]), Fragment] = {
        val apply = (tags :+ tag).sumr
        emit(fragment).filter(_ => apply.keep(arguments, apply.names)) fby go(tags)
      }

      def skip(fragment: Fragment) =
        emit(fragment).filter(_ => false) fby go(sections)

      receive1 {
        // 1. tag for the next fragment, after some blank text
        case (Some(Fragment(Marker(t, false, true),_,_)), Fragment(Text(tx), _, _), Some(fragment)) if tx.trim.isEmpty =>
          filter(fragment, tag = t)

        // 2. don't emit the fragment twice if it has already been emitted with condition 1.
        case (Some(Fragment(Text(tx), _, _)), fragment, _) if tx.trim.isEmpty =>
          skip(fragment)

        // 3. otherwise if the tag is for the next fragment
        case (Some(Fragment(Marker(t, false, true),_,_)), fragment, _) =>
          filter(fragment, tag = t)

        // 4. tag for the previous fragment with one blank in between
        case (Some(fragment), Fragment(Text(tx), _, _), Some(Fragment(Marker(t, false, false),_,_))) if tx.trim.isEmpty =>
          filter(fragment, tag = t)

        // 5. tag for the previous fragment
        case (_, fragment, Some(Fragment(Marker(t, false, false),_,_))) =>
          filter(fragment, tag = t)

        // 6. no tag after a fragment and blank text, emit it
        case (Some(fragment), Fragment(Text(tx), _, _), _) if tx.trim.isEmpty =>
          filter(fragment)

        // 7. if the next fragment is some empty text, don't emit the tag right away, wait for condition 4.
        case (_, fragment, Some(Fragment(Text(tx), _, _))) if tx.trim.isEmpty =>
          skip(fragment)

        // 8. start or end of a new section
        case (Some(Fragment(Marker(tag, true, true), _, _)), fragment, _) =>
          filter(fragment, updateSections(tag))

        // 9. start or end of a new section impacting the current fragment
        case (_, fragment, Some(Fragment(Marker(tag, true, false), _, _))) =>
          filter(fragment, updateSections(tag), tag)

        // 10. filter fragments in between tags according to sections
        case (_, fragment, _) =>
          filter(fragment)
      }
    }

    if ((arguments.include + arguments.exclude).nonEmpty) withPreviousAndNext[Fragment] |> go()
    else process1.id[Fragment]
  }

  /**
   * filter fragments by previous execution and required status
   */
  def filterByPrevious(env: Env): Process1[Fragment, Fragment] =
    if (env.arguments.wasIsDefined) process1.filter((_: Fragment).was(env.arguments.was))
    else process1.id[Fragment]

}

object DefaultSelector extends DefaultSelector
