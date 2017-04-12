package org.specs2.matcher.describe

/**
 * Import the implicit Diffable contained in this object to get a colored output
 * showing line differences in Strings containing lots of lines
 */
object LinesDiffable {

  implicit val largeStringDiffable: Diffable[String] = new Diffable[String] {
    def diff(actual: String, expected: String): ComparisonResult = {
      val (actualLines, expectedLines) =
        (actual.toString.split("\n").toList,
          expected.toString.split("\n").toList)

      if (actualLines.size + expectedLines.size > 2)
        linesDiffable[String].diff(actualLines, expectedLines)
      else
        Diffable.stringDiffable.diff(actual, expected)
    }
  }

  implicit def linesDiffable[T : Diffable]: Diffable[List[T]] = new Diffable[List[T]] {
    def diff(actual: List[T], expected: List[T]): ComparisonResult =
      LinesComparisonResult(actual, expected)
  }

}

case class LinesComparisonResult[T : Diffable](actual: List[T], expected: List[T]) extends ComparisonResult {
  import org.specs2.data._
  import EditDistance._
  import org.specs2.text.AnsiColors._

  val operations: IndexedSeq[EditDistanceOperation[T]] =
    levenhsteinDistance[T](actual.toIndexedSeq, expected.toIndexedSeq)(new Equiv[T] {
      def equiv(a: T, b: T) = implicitly[Diffable[T]].diff(a, b).identical
    })

  def identical: Boolean =
    operations.forall {
      case _:Same[_] => true
      case _ => false
    }

  def render: String = operations.flatMap {
    case Same(line)          => List(line)
    case Add(line)           => List(color("+ "+line, green))
    case Del(line)           => List(color("- "+line, red))
    case Subst(line1, line2) => List(color("- "+line1, red), color("+ "+line2, green))
  }.mkString("\n", "\n", "\n")

}
