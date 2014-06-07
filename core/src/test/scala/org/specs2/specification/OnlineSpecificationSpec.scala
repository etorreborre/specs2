package org.specs2
package specification

import execute._
import scalaz.stream.Process
import reporter.TextPrinterSpec._
import core.{FragmentsContinuation, Execution, RawText}

class OnlineSpecificationSpec extends Specification { def is = s2"""

 A specification can have examples returning a result and Fragments depending on the result value $e1

"""
  import dsl1._

  def e1 = {
    def continue(n: Int): FragmentsContinuation = FragmentsContinuation { r: Result =>
      if (n == 1) None
      else        Some(core.Fragments(Process.emit(break) fby createExample(n - 1).contents))
    }

    def online(n: Int) = Execution(success, continue(n))

    def createExample(n: Int) = core.Fragments(fragmentFactory.example(RawText("an online example"), online(n)))

    createExample(3) contains
      """|[info] + an online example
         |[info] + an online example
         |[info] + an online example""".stripMargin
  }

}
