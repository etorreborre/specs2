package org.specs2
package specification
package script

import collection.Seqx._
import text.Trim._
import TagsFragments.{Tag, Section}
import execute.AsResult

abstract class Specification extends SpecificationLike

trait SpecificationLike extends org.specs2.Specification with Scripts with GroupsLike { outer =>

  override def map(fs: =>Fragments) = {
    val script = GroupsScript()
    script.groupTemplate.lines(super.map(fs.compact), script).fs
  }

  case class GroupsScript(title: String = "groups", isStart: Boolean = true, groups: GroupsLike = outer)
                         (implicit template: ScriptTemplate[GroupsScript, FragmentsScriptLines] = BulletedExamplesTemplate()) extends Script {

    val groupTemplate = template

    def fragments(text: String): Fragments = template.lines(Fragments.createList(Text(text)), this).fs

    def start = this
    def end = copy(isStart = false)
  }

  case class FragmentsScriptLines(fs: Fragments) extends ScriptLines

  case class BulletedExamplesTemplate(marker: String = "+") extends ScriptTemplate[GroupsScript, FragmentsScriptLines] {
    def lines(fs: Fragments, script: GroupsScript) = {

      val result =
        fs.compact.middle.foldLeft((fs.copy(middle = Seq()), 0, 0)) { (res, cur) =>
          val (resultFragments, groupIndex, exampleIndex) = res
          val (fragments, newGroupIndex, newExampleIndex) =
            cur match {
              case Text(t) => examplify(t, script.groups, groupIndex, exampleIndex)
              case other   => (Fragments.createList(other), groupIndex, exampleIndex)
            }
          (resultFragments ^ fragments, newGroupIndex, newExampleIndex)
      }._1.compact

      FragmentsScriptLines(result)
    }

    def examplify(text: String, groups: GroupsLike, groupIndex: Int, exampleIndex: Int): (Fragments, Int, Int) = {

      /** match input fragments and group examples */
      def setBodies(fs: Seq[Fragment]): (Fragments, Int, Int) = {
        fs.foldLeft((Fragments.createList(), groupIndex, exampleIndex)) { (res, cur) =>
          val (fragments, i, j) = res
          def groupTagsFor(i: Int) = {
            val name = group(i).groupName
            if (name.matches("g\\d\\d?\\.e\\d\\d?")) Seq(Section(name))
            else                                     Seq(Section(name.removeEnclosing("'"), s"g${i+1}"))
          }

          def exampleName(i: Int, j: Int) = s"g${i+1}.e${j+1}"
          def createExample(line: String, i: Int, j: Int) =
            exampleFactory.newExample(strip(line), (groups.group(i).example(j).t()).mapMessage(_ + " - " + exampleName(i, j)))

          val (groupTags, exampleTags) = ((if (j == 0) groupTagsFor(i) else Seq()), Seq(Tag(exampleName(i, j))))

          val (newFragments, newi, newj) =
            cur match {
              case Example(line,_) => (groupTags ++ (indentation(line.toString) +: exampleTags :+ createExample(line.toString, i, j) :+ Text("\n")), i, j+1)
              case other           => (groupTagsFor(i) :+ other, i + 1, 0)
            }
          (fragments append newFragments, newi, newj)
        }
      }
      val lines = text.split("\n").toSeq
      val linesWithNewLines = lines.map(_ + "\n").updateLast(_.removeLast("\n"))
      val fragments = linesWithNewLines.foldLeft(Fragments.createList()) { (res, line) =>
        res append Seq(if (isExample(line)) exampleFactory.newExample(line, execute.Pending()) else Text(line))
      }.compact

      fragments.middle match {
        case Text(t) +: rest => {
          val (bodies, newGroupIndex, newExampleIndex) = setBodies(rest)
          (Fragments.createList((Text(t) +: bodies.middle):_*), newGroupIndex, newExampleIndex)
        }
        case other           => setBodies(other)
      }

    }

    def isExample(line: String) = line.trim.startsWith(marker)
    def strip(line: String) = line.trim.removeFirst(s"\\Q$marker\\E")
    def indentation(line: String) = Text(line.takeWhile(_ == ' ').mkString)
  }

}
