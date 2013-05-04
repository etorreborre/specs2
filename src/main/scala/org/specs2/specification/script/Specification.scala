package org.specs2
package specification
package script

import collection.Seqx._
import text.Trim._
import TagsFragments.{Tag, Section}
import execute.AsResult

abstract class Specification extends SpecificationLike

trait SpecificationLike extends org.specs2.Specification with Scripts with GroupsLike { outer =>

  override def map(fs: =>Fragments) = GroupsScript().lines(super.map(fs.compact))

  case class GroupsScript(title: String = "groups", isStart: Boolean = true, groups: GroupsLike = outer, groupIndex: Int = 0, exampleIndex: Int = 0)
                         (implicit template: ScriptTemplate[GroupsScript, FragmentsScriptLines] = BulletedExamplesTemplate()) extends Script {

    val groupTemplate = template

    def fragments(text: String): Fragments = template.lines(text, this).fs

    def lines(fs: Fragments) = {
      fs.compact.middle.foldLeft(FragmentsScriptLines(fs.copy(middle = Seq()), 0, 0)) { (res, cur) =>
        val FragmentsScriptLines(resultFragments, oldGroupIndex, oldExampleIndex) = res
        val FragmentsScriptLines(fragments, newGroupIndex, newExampleIndex) =
          cur match {
            case Text(t) => groupTemplate.lines(t, copy(groupIndex = oldGroupIndex, exampleIndex = oldExampleIndex))
            case other   => FragmentsScriptLines(Fragments.createList(other), oldGroupIndex, oldExampleIndex)
          }
        FragmentsScriptLines(resultFragments ^ fragments, newGroupIndex, newExampleIndex)
      }.fs.compact
    }

    def group(i: Int) = groups.group(i)

    def start = this
    def end = copy(isStart = false)
  }

  case class FragmentsScriptLines(fs: Fragments, groupIndex: Int, exampleIndex: Int) extends ScriptLines {
    def appendText(t: String) = copy(fs = Fragments.createList((Text(t) +: fs.middle):_*))
  }

  case class BulletedExamplesTemplate(marker: String = "+") extends ScriptTemplate[GroupsScript, FragmentsScriptLines] {

    def lines(text: String, script: GroupsScript): FragmentsScriptLines = {

      /** match input fragments and group examples */
      def setBodies(fs: Seq[Fragment]) = {
        fs.foldLeft(FragmentsScriptLines(Fragments.createList(), script.groupIndex, script.exampleIndex)) { (res, cur) =>
          val FragmentsScriptLines(fragments, i, j) = res
          def groupTagsFor(i: Int) = {
            val name = script.group(i).groupName
            if (name.matches("g\\d\\d?\\.e\\d\\d?")) Seq(Section(name))
            else                                     Seq(Section(name.removeEnclosing("'"), s"g${i+1}"))
          }

          def exampleName(i: Int, j: Int) = s"g${i+1}.e${j+1}"
          def createExample(line: String, i: Int, j: Int) =
            exampleFactory.newExample(strip(line), (script.group(i).example(j).t()).mapMessage(_ + " - " + exampleName(i, j)))

          val (groupTags, exampleTags) = ((if (j == 0) groupTagsFor(i) else Seq()), Seq(Tag(exampleName(i, j))))

          val (newFragments, newi, newj) =
            cur match {
              case Example(line,_) => (groupTags ++ (indentation(line.toString) +: exampleTags :+ createExample(line.toString, i, j) :+ Text("\n")), i, j+1)
              case other           => (groupTagsFor(i) :+ other, i + 1, 0)
            }
          FragmentsScriptLines(fragments append newFragments, newi, newj)
        }
      }

      val lines = text.split("\n").toSeq
      val linesWithNewLines = lines.map(_ + "\n").updateLast(_.removeLast("\n"))
      val fragments = linesWithNewLines.foldLeft(Fragments.createList()) { (res, line) =>
        res append Seq(if (isExample(line)) exampleFactory.newExample(line, execute.Pending()) else Text(line))
      }.compact

      fragments.middle match {
        case Text(t) +: rest => setBodies(rest).appendText(t)
        case other           => setBodies(other)
      }

    }

    def isExample(line: String) = line.trim.startsWith(marker)
    def strip(line: String) = line.trim.removeFirst(s"\\Q$marker\\E")
    def indentation(line: String) = Text(line.takeWhile(_ == ' ').mkString)
  }

}
