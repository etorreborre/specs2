package org.specs2
package specification
package script


/**
 * The bullet template associates lines starting with `*` and a keyword `given` / `when` ou `then`
 * to given / when / then steps
 */
case class BulletTemplate(bullet: String = "*") extends ScriptTemplate[Scenario, GivenWhenThenLines] {
  def lines(text: String, script: Scenario): GivenWhenThenLines = {
    text.split("\n").foldLeft(GivenWhenThenLines()) { (res, line) =>
      val firstBulletWord =
        (if (line.trim.startsWith(bullet)) line.trim.drop(1).trim.split(" ").headOption.getOrElse("") else "").toLowerCase

      val newLine = line.replace(bullet+" ", "")
      if      (firstBulletWord.startsWith("given")) res.append(GivenLines.create(newLine))
      else if (firstBulletWord.startsWith("when"))  res.append(WhenLines.create(newLine))
      else if (firstBulletWord.startsWith("then"))  res.append(ThenLines.create(newLine))
      else                                          res.append(TextLines(line))
    }
  }
}
