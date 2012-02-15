package org.specs2
import net.rosien.sniff._

class SmellsSpec extends Specification {


  implicit val ignore = Ignores(
    Ignore('NoLongLine, "src/main/scala/org/specs2/matcher/DataTable.scala",
                        "src/main/scala/org/specs2/reporter/ConsoleNotifier.scala",
                        "src/main/scala/org/specs2/matcher/ScalaCheckMatchers.scala")
  )

  val IgnoredSmells = Set('NoJUnit, 'NoSpecs1, 'NoURL, 'NoThreadSleep)

  def is = "There should be no code smells" ^
  ok
    //CodeSnippets(Scala, Scala.snippets.smells.filterNot(s => IgnoredSmells(s.id)):_*).sniff("src/main/scala", "src/test/scala")

}
