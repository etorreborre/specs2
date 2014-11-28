package org.specs2
package guide

import execute.{Result, Success, AsResult}
import specification.{Fixture, Around}

object ContextObjects extends UserGuidePage { def is = s2"""

In the ${see(Contexts)} section we have seen how to create contexts for each example in a specification. Whilst you generally want to group all the examples having the same kind of setup in the same specification, this is not always the case. So if you have a situation where you just need to create a "local" context for just a few examples here is what you can do.

### Use case classes

In an acceptance specification you can simply use case classes to get a "fresh" context on some examples: ${snippet{
class ContextSpec extends Specification { def is = s2"""
  this is the first example                          ${trees().e1}
  this is the second example                         ${trees().e2}
"""

  case class trees() {
    val tree = createATreeWith4Nodes

    // each example has access to a brand new tree object
    def e1 = tree.removeNodes(2, 3) must have size(2)
    def e2 = tree.removeNodes(2, 3, 4) must have size(1)
  }
}
}}

If you also want to include some setup/cleanup behavior you can use the `Before` or `After` traits (or `BeforeAfter` or `Around`): ${snippet{
class ContextSpec extends Specification { def is = s2"""
  this is the first example                          ${trees().e1}
  this is the second example                         ${trees().e2}
"""

  case class trees() extends specification.After {
    lazy val tree = getATreeWith4NodesFromTheDatabase

    // you need to define the "after" method
    def after = cleanupDB()

    // this is equivalent to: def e1 = this.apply { ... }
    def e1 = this { tree.removeNodes(2, 3) must have size(2) }
    def e2 = this { tree.removeNodes(2, 3, 4) must have size(1) }
  }
}
}}

As you can see the `Before`, `After`,... traits are very similar to their `BeforeEach`, `AfterEach`,... counterparts. One good thing about this technique is that each example has access to the current state being set (a bit like when using the `ForEach` trait).

### Context object

A slightly different technique consists in creating an object extending `Before`, `After`, `BeforeAfter` or `Around` so that you can reuse it independently of examples:${snippet{
object http extends Around {
  def around[T : AsResult](t: =>T) = openHttpSession("test") {
    AsResult(t)  // execute t inside a http session
  }
}

s2"""
this is a first example where the code executes inside a http session ${http(e1)}
and another one                                                       ${http(e2)}
"""

def e1 = ok // do something
def e2 = ok // here too

}}

This works because each "context" object has an `apply` method taking `R : AsResult` and returning `Result`.

Finally a last kind of "context" object, a `Fixture` can be used to inject some state:${snippet{
val evenNumbers = new Fixture[Int] {
  def apply[R : AsResult](f: Int => R) = {
    // test f with 1, 2, 3
    Seq(1, 2, 3).foldLeft(Success(): Result) { (res, i) =>
      res and AsResult(f(i))
    }
  }
}

s2"even numbers can be divided by 2  $e1"

def e1 = evenNumbers { i: Int => i % 2 === 0 }
}}

"""

  case class Tree[T](ts: T*) {
    def removeNodes(n: Int*) = Seq[Int]()
  }
  def cleanupDB() = ()
  def createATreeWith4Nodes = new Tree()
  def getATreeWith4NodesFromTheDatabase = new Tree()
  def openHttpSession[R : AsResult](name: String)(r: =>Result) = r

}
