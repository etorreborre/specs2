package user.reporter

import org.specs2._
import org.specs2.main._

class TestSpec extends Specification { def is = args.report(diffs = ArgProperty(NewDiffs())) ^ s2"""

 test $e1

"""

  def e1 = {
    val address1 = Address(1, StreetName("street1"))
    val address2 = Address(1, StreetName("street2"))
    val p1 = Person("name1", address1)
    val p2 = Person("name2", address2)

    p1 must_== p1
  }
}

case class Person(name: String, address: Address)
case class Address(number: Int, street: StreetName)
case class StreetName(name: String)

case class NewDiffs() extends Diffs {
  val smart = SmartDiffs()

  def show: Boolean = smart.show
  def show(actual: Any, expected: Any): Boolean = smart.show(actual, expected)
  def showDiffs(actual: Any, expected: Any): (String, String) = smart.showDiffs(actual, expected)

  def showSeq(actual: Seq[Any], expected: Seq[Any], ordered: Boolean): Boolean = smart.showSeq(actual, expected, ordered)
  def showMap(actual: Map[Any, Any], expected: Map[Any, Any]): Boolean = smart.showMap(actual, expected)

  def showSeqDiffs(actual: Seq[Any], expected: Seq[Any], ordered: Boolean): (Seq[String], Seq[String]) = smart.showSeqDiffs(actual, expected, ordered)
  def showMapDiffs(actual: Map[Any, Any], expected: Map[Any, Any]): (Seq[String], Seq[String], Seq[String]) = smart.showMapDiffs(actual, expected)
  def showFull: Boolean = smart.showFull

}