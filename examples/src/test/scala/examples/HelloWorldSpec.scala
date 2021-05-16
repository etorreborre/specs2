package examples

import org.specs2.*
import org.specs2.main.*

/**
 * This specification shows how to create examples using the "acceptance" style
 */
class HelloWorldSpec extends Specification:
  def is = s2"""

This is a specification to check the 'Hello world' string

The 'Hello world' string should
  contain 11 characters $e1 ${t(2)}
    have more examples, nested $e1
    have more examples, nested $e1
  start with 'Hello' $e2
  end with 'world' $e3

  show diffs $e4

  """

  def e1 = "Hello world" must haveSize(11)
  def e2 = "Hello world" must startWith("Hello")
  def e3 = "Hello world" must endWith("world")

  def e4 = {
    // List.fill(50)(Person("alice")) === List.fill(50)(Person("bob"))
    "abcdefghijklmnopqrstuvwxyz" === "abcdufghijklmnopqrstevwxyz"
  }

  case class Person(name: String)

class MyDiffs extends Diffs:
  /** @return true if the differences must be shown */
  def show: Boolean = true
  /** @return true if the differences must be shown for 2 different values */
  def show(actual: Any, expected: Any): Boolean = true
  /** @return true if the differences must be shown for 2 different sequences of values */
  def showSeq(actual: Seq[Any], expected: Seq[Any], ordered: Boolean): Boolean = true
  /** @return true if the differences must be shown for 2 different maps */
  def showMap(actual: Map[Any, Any], expected: Map[Any, Any]): Boolean = true
  /** @return the diffs */
  def showDiffs(actual: Any, expected: Any): (String, String) = ("ACTUAL! "+actual, "EXPECTED! "+expected)
  /** @return the diffs for sequences with missing / added values  */
  def showSeqDiffs(actual: Seq[Any], expected: Seq[Any], ordered: Boolean): (Seq[String], Seq[String]) = (actual.map(_.toString), expected.map(_.toString))
  /** @return the diffs for sequences with missing / added values  */
  def showMapDiffs(actual: Map[Any, Any], expected: Map[Any, Any]): (Seq[String], Seq[String], Seq[String]) = (Nil, Nil, Nil)
  /** @return true if the full strings must also be shown */
  def showFull: Boolean = true
