package org.specs2
package specification
package script

import collection.Seqx._
import create._
import core._
import text.Trim._

/**
 * This Specification trait is using a Script (`GroupScript`) and a default template `BulletedExamplesTemplate`
 * to associate extracted examples text (where there are `+` signs) to example bodies coming from a `Group`.
 * tod
 */
abstract class Specification extends SpecificationLike

/**
 * Trait for the script.Specification abstract class
 */
trait SpecificationLike extends org.specs2.SpecificationLike with Scripts with GroupsLike { outer =>

  /** analyse the fragments and extract examples from pieces of text */
  override def map(fs: =>Fragments) = GroupsScript(groups = outer)(BulletedExamplesTemplate(fragmentFactory), fragmentFactory).lines(super.map(fs))
}

/**
 * This script associates lines extracted by a template to example bodies defined by a GroupsLike trait.
 *
 * It can be called several times with a groupIndex and exampleIndex to know exactly which groups and examples it should
 * start associating
 */
case class GroupsScript(title: String = "groups", isStart: Boolean = true, groups: GroupsLike)
                       (implicit template: ScriptTemplate[GroupsScript, FragmentsScriptLines], factory: FragmentFactory) extends Script {

  val groupTemplate = template

  def fragments(text: String): Fragments = createExamples(template.lines(text, this), 0, 0)._1

  /**
   * Go through the list of all fragments. For each piece of text, try to parse it with the template
   * and replace it with new fragments containing examples by associating the marked text with groups examples
   */
  def lines(fs: Fragments) = {
    fs.fragments.foldLeft((Fragments(), 0, 0)) { (res, cur) =>
      val (resultFragments, previousGroupIndex, previousExampleIndex) = res
      val (fragments, newGroupIndex, newExampleIndex) =
        cur match {
          case t: Fragment if Fragment.isText(t) => createExamples(groupTemplate.lines(t.description.show, this), previousGroupIndex, previousExampleIndex)
          case other   => (Fragments(other), previousGroupIndex, previousExampleIndex)
        }
      (resultFragments append fragments, newGroupIndex, newExampleIndex)
    }._1
  }

  /** match input fragments and group examples */
  private def createExamples(fragmentLines: FragmentsScriptLines, groupIndex: Int, exampleIndex: Int) = {
    fragmentLines.blocks.foldLeft((Fragments(), groupIndex, exampleIndex)) { (res, block) =>
      val (fragments, g, e) = res
      (fragments append createExamplesForBlock(block, g, e), if (block.fragments.exists(Fragment.isExample)) g + 1 else g, 0)
    }
  }

  private def createExamplesForBlock(block: Fragments, groupIndex: Int, exampleIndex: Int) = {
    groupTagsFor(groupIndex) ++
    block.fragments.foldLeft((Seq[Fragment](), exampleIndex)) { (res, cur) =>
      val (fragments, e) = res

      cur match {
        case ex if Fragment.isExample(ex) =>
          (fragments ++ (indentation(ex.description.show) +: exampleTagsFor(groupIndex, e) :+ createExample(ex.description.show, groupIndex, e)), e + 1)

        case other =>
          (fragments :+ factory.break :+ other, e)
      }
    }._1 ++
    groupTagsFor(groupIndex)
  }

  private def group(i: Int) = groups.createExamplesGroup(i)

  private def exampleTagsFor(g: Int, e: Int) = Seq(factory.taggedAs(exampleName(g, e)))

  private def groupTagsFor(i: Int) = {
    val name = group(i).groupName
    if (name.matches("g\\d\\d?\\.e\\d\\d?")) Seq(factory.section(name))
    else                                     Seq(factory.section(name.removeEnclosing("'"), s"g${i+1}"))
  }

  private def exampleName(i: Int, j: Int) = s"g${i+1}.e${j+1}"
  private def createExample(line: String, i: Int, j: Int) = factory.example(line, group(i).createExample(j).execution().mapMessage(_ + " - " + exampleName(i, j)))
  private def indentation(line: String) = factory.text(line.takeWhile(_ == ' ').mkString)

  def start = this
  def end = copy(isStart = false)
}

case class FragmentsScriptLines(blocks: Seq[Fragments]) extends ScriptLines

trait GroupTemplateParameters {
  def isExample(line: String): Boolean
  def isGroupStart(line: String, nextLine: String): Boolean
  def stripExample(line: String): String
  def stripGroup(line: String): String
}
case class BulletedExamplesTemplateParameters() extends GroupTemplateParameters {
  def isExample(line: String) = line.trim.startsWith("+")

  def isGroupStart(line: String, nextLine: String) =
    Seq("====", "----").exists(nextLine.trim.startsWith) ||
    line.trim.matches("#+.*")                            ||
    line.trim.matches("<h\\d/>.*")                       ||
    line.trim.matches("<h\\d>.*?</h\\d>")

  def stripExample(line: String) = line.trim.removeFirst(s"\\s*\\+ ")
  def stripGroup(line: String) = line
}

case class BulletedExamplesTemplate(factory: FragmentFactory)(implicit params: GroupTemplateParameters = BulletedExamplesTemplateParameters()) extends ScriptTemplate[GroupsScript, FragmentsScriptLines] {

  def lines(text: String, script: GroupsScript): FragmentsScriptLines = {
    val lines = text.split("\n").toSeq
    val linesWithNewLines = lines.map(_ + "\n").updateLast(_.removeLast("\n"))

    val fragmentLines = linesWithNewLines.zipWithIndex.foldLeft(Seq(Fragments())) { (res, linei) =>
      val (line, index) = linei
      val nextLine = if (index + 1 < linesWithNewLines.size) linesWithNewLines(index + 1) else ""

      val blocks = if (startNewBlock(line, nextLine, res.lastOption)) res :+ Fragments() else res
      blocks.updateLast(fs => fs append createFragments(line))
    }
    FragmentsScriptLines(fragmentLines)
  }

  private def startNewBlock(line: String, nextLine: String, lastBlock: Option[Fragments]) =
    params.isGroupStart(line, nextLine) && lastBlock.fold(false)(_.fragments.exists(Fragment.isExample))

  private def createFragments(line: String) =
    if (params.isExample(line)) Fragments(factory.text(line.takeWhile(_ == ' ')), factory.example(params.stripExample(line), execute.Pending()))
    else                        Fragments(factory.text(params.stripGroup(line)))
}
