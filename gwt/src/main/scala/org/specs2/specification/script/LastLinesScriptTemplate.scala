package org.specs2
package specification
package script


/**
 * The LastLines template takes the number of given / when / then steps of the scenario and associate the last (non-empty)
 * lines of the text with them
 */
case class LastLinesScriptTemplate() extends ScriptTemplate[Scenario, GivenWhenThenLines] {
  def lines(text: String, script: Scenario) = {
    // reverse the text and put the last empty lines aside
    val reversed = text.split("\n").reverse.toSeq
    val (emptyLines, ls) = reversed.span(_.trim.isEmpty)

    val linesBlocks = Seq(GivenLines(_), WhenLines(_), ThenLines(_))

    val grouped = (script.stepsNumbers zip linesBlocks).reverse.foldLeft((GivenWhenThenLines(), ls.reverse)) { (res, cur) =>
      val (gwtLines, remainingLines) = res
      val (stepsNumber, createBlock) = cur

      (gwtLines.prepend(createBlock(remainingLines.takeRight(stepsNumber).toVector)), remainingLines.dropRight(stepsNumber))
    }
    // add the remaining lines at the beginning and the empty lines at the end
    grouped._1.prepend(TextLines(grouped._2.mkString("\n"))).append(TextLines.create(emptyLines))
  }
}

