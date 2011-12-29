package org.specs2
package specification

import io._
import reporter._
import ExecutingSpecificationData._

class ExecutionModelSpec extends Specification with ScalaCheck { def is =

  "A sequential specification must always be reported with a strict order of its fragments"                             ! e1^
  "A concurrent specification must have at least one execution where the order is changed"                              ! e2^
  "A Step must always be executed after the preceding examples "                                                        ! e3^
                                                                                                                        end

  val exporter = new TextExporting with MockOutput

  def e1 = check { spec: ExecutingSpecification =>
    exporter.export(sequential)(label(spec))
    exporter.messages must beSorted
  }

  def label(spec: ExecutingSpecification) = {
    var i = 0
    spec.map { body =>
      i = i+1
      println("label "+i)
      body
    }
  }

  def e2 = pending
  def e3 = pending

}