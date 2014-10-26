package org.specs2
package data

import scalaz.Reducer
import scalaz.concurrent.Task
import scalaz.stream.Process
import scalaz._, Scalaz._
import matcher._

class FoldSpec extends Spec with ScalaCheck with TaskMatchers { def is = s2"""

 A Fold can be executed on a large Process $large
 A Fold can be created from a Reducer      $reducer1

 """

  def large = {
    var n = 0
    val max = 500000
    val fold = Fold.fromFunction[Int](i => Task.delay(n = n+1))
    Fold.runFold(Process.range(0, max), fold).run
    n must_== max
  }

  def reducer1 = {
    val reducer = Reducer.unitReducer((_:String).size)
    var result = 0
    val fold: Fold[String] = Fold.fromReducerAndLast[String, Int](reducer, (i: Int) => Task.delay(result = i))
    Fold.runFold(Process.emitAll(Seq("ab", "c", "def")).toSource, fold) must returnOk
    result must_== 6
  }
}
