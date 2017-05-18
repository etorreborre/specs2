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
  override def map(fs: =>Fragments): Fragments =
    super.map(fs.compact).mapFragments { fs =>
      GroupsScript(groups = outer)(BulletedExamplesTemplate(fragmentFactory), fragmentFactory).
      lines(fs)
    }
}

abstract class Spec extends SpecLike

/**
 * Trait for the script.Spec abstract class
 */
trait SpecLike extends org.specs2.Spec with Scripts with GroupsLike { outer =>
  /** analyse the fragments and extract examples from pieces of text */
  override def map(fs: =>Fragments): Fragments =
    super.map(fs.compact).mapFragments { fragments =>
      GroupsScript(groups = outer)(BulletedExamplesTemplate(fragmentFactory), fragmentFactory).lines(fragments)
    }
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

  def fragments(text: String): FragmentsSeq =
    createExamples(template.lines(text, this), 0, 0)._1

  /**
   * Go through the list of all fragments. For each piece of text, try to parse it with the template
   * and replace it with new fragments containing examples by associating the marked text with groups examples
   */
  def lines(fs: List[Fragment]): List[Fragment] = {
    fs.foldLeft((FragmentsSeq.empty, 0, 0)) { (res, cur) =>
      val (resultFragments, previousGroupIndex, previousExampleIndex) = res
      val (fragments, newGroupIndex, newExampleIndex) =
        cur match {
          case t if Fragment.isText(t) && t.description.show.trim.nonEmpty =>
            createExamples(groupTemplate.lines(t.description.show, this), previousGroupIndex, previousExampleIndex)

          case other =>
            (FragmentsSeq(other), previousGroupIndex, previousExampleIndex)
        }
      (resultFragments ++ fragments, newGroupIndex, newExampleIndex)
    }._1.fs.toList
  }

  /** match input fragments and group examples */
  private def createExamples(fragmentLines: FragmentsScriptLines, groupIndex: Int, exampleIndex: Int): (FragmentsSeq, Int, Int) = {
    fragmentLines.blocks.foldLeft((FragmentsSeq.empty, groupIndex, exampleIndex)) { (res, block) =>
      val (fragments, g, e) = res
      (fragments append createExamplesForBlock(block, g, e), if (block.fs.exists(Fragment.isExample)) g + 1 else g, 0)
    }
  }

  private def createExamplesForBlock(block: FragmentsSeq, groupIndex: Int, exampleIndex: Int): FragmentsSeq = {
    groupTagsFor(groupIndex) ++
    block.fs.foldLeft((FragmentsSeq.empty, exampleIndex)) { (res, cur) =>
      val (fragments, e) = res

      cur match {
        case ex if Fragment.isExample(ex) =>
          (fragments ++
            FragmentsSeq(indentation(ex.description.show)) ++
            exampleTagsFor(groupIndex, e) :+
            createExample(ex.description.show, groupIndex, e), e + 1)

        case other =>
          (fragments :+ factory.break :+ other, e)
      }
    }._1 ++
    groupTagsFor(groupIndex)
  }

  private def group(i: Int): ExamplesGroup =
    groups.createExamplesGroup(i)

  private def exampleTagsFor(g: Int, e: Int): FragmentsSeq =
    FragmentsSeq(factory.tag(exampleName(g, e)))

  private def groupTagsFor(i: Int): FragmentsSeq = {
    val name = group(i).groupName
    if (name.matches("g\\d\\d?\\.e\\d\\d?")) FragmentsSeq(factory.section(name))
    else                                     FragmentsSeq(factory.section(name.removeEnclosing("'"), s"g${i+1}"))
  }

  private def exampleName(i: Int, j: Int) = s"g${i+1}.e${j+1}"
  private def createExample(line: String, i: Int, j: Int) = factory.example(line, group(i).createExample(j).execution().mapMessage(_ + " - " + exampleName(i, j)))
  private def indentation(line: String) = factory.text(line.takeWhile(_ == ' ').mkString)

  def start = this
  def end = copy(isStart = false)
}

/**
 * Block of fragments
 */
case class FragmentsScriptLines(blocks: Vector[FragmentsSeq]) extends ScriptLines

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

/**
 * Analyse a piece of text and group Fragments belonging to the same group
 */
case class BulletedExamplesTemplate(factory: FragmentFactory)(implicit params: GroupTemplateParameters = BulletedExamplesTemplateParameters()) extends ScriptTemplate[GroupsScript, FragmentsScriptLines] {

  def lines(text: String, script: GroupsScript): FragmentsScriptLines = {
    val lines = text.split("\n").toSeq
    val linesWithNewLines = lines.map(_ + "\n").updateLast(_.removeLast("\n"))

    val fragmentLines = linesWithNewLines.zipWithIndex.foldLeft(Vector.empty[FragmentsSeq]) { (res, linei) =>
      val (line, index) = linei
      val nextLine = if (index + 1 < linesWithNewLines.size) linesWithNewLines(index + 1) else ""

      val blocks = if (startNewBlock(line, nextLine, res.lastOption)) res :+ FragmentsSeq.empty else res
      blocks.updateLast(fs => fs ++ createFragments(line)).toVector
    }
    FragmentsScriptLines(fragmentLines.map(_.compact))
  }

  private def startNewBlock(line: String, nextLine: String, lastBlock: Option[FragmentsSeq]): Boolean =
    params.isGroupStart(line, nextLine) && lastBlock.exists(_.fs.exists(Fragment.isExample))

  private def createFragments(line: String): FragmentsSeq =
    if (params.isExample(line)) FragmentsSeq(factory.text(line.takeWhile(_ == ' ')), factory.example(params.stripExample(line), execute.Pending()))
    else                        FragmentsSeq(factory.text(params.stripGroup(line)))
}

/**
 * List of fragments with utility functions to manipulate it
 * @param fs
 */
case class FragmentsSeq(fs: Vector[Fragment]) {

  def toFragments: Fragments =
    Fragments.apply(fs:_*)

  def append(fragment: Fragment): FragmentsSeq =
    FragmentsSeq(fs :+ fragment)

  def append(other: FragmentsSeq): FragmentsSeq =
    FragmentsSeq(fs ++ other.fs)

  def append(other: Seq[Fragment]): FragmentsSeq =
    FragmentsSeq(fs ++ other.toVector)

  def ++(other: FragmentsSeq): FragmentsSeq =
    append(other)

  def :+(fragment: Fragment): FragmentsSeq =
    FragmentsSeq(fs :+ fragment)

  def compact: FragmentsSeq =
    this
}

object FragmentsSeq {

  val empty: FragmentsSeq =
    FragmentsSeq(Vector.empty)

  def apply(f: Fragment, fs: Fragment*): FragmentsSeq =
    FragmentsSeq(f +: fs.toVector)
}
