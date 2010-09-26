package org.specs2
package reporter
import scalaz._
import Scalaz._
import specification._

class SpecificationTreeSpec extends SpecificationWithJUnit with SpecificationTree {
  val examples = 
  "a specification can be turned to a tree of fragments"^
  "if there is only one text fragment, the tree will have only one leaf" ! {
	toTree("name", List(Text("description"))) must_== node(Text("name"), Stream(leaf(Text("description"))))
  }
}