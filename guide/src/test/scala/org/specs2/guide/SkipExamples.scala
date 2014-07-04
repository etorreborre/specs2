package org.specs2
package guide

object SkipExamples extends UserGuidePage { def is = s2"""
The section on ${"standard results" ~/ StandardResults} already presents 2 methods for skipping examples. But this works for individual examples only. In some circumstances you might want to skip a whole specification. For example, your specification needs to access a web service in order to work and this service might not be available on all testing machines. You can skip this specification conditionally with the `skipAllIf` argument:${snippet{
class InactiveSpec extends Specification { def is = skipAllIf(databaseIsDown) ^ s2"""
  There is a list of customers in the database $e1
  One of them is called Eric                   $e2
"""

  def e1 = database.getCustomers must not beEmpty
  def e2 = database.getCustomers must contain((_:Customer).name === "Eric")

}
}}

There also is a version of `skipAllIf` which reads better for some conditions: `skipAllUnless`.
"""
  case class Customer(name: String)
  object database {
    def getCustomers: List[Customer] = ???
  }

  lazy val databaseIsDown = true
}