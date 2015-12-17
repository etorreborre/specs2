package org.specs2
package specification

import org.specs2.control._
import org.specs2.main.Arguments
import org.specs2.reporter.LineLogger._
import org.specs2.specification.core.{SpecificationStructure, Env}
import runner._
import scala.collection.mutable.ArrayBuffer

class BeforeAfterAllSpec extends Specification { def is = s2"""

 Before and after all steps can be executed with the BeforeAfterAll trait $beforeAfter
 Before and after all steps can be executed even if tags are included     $withTags1
 Before and after all steps can be executed even if tags are excluded     $withTags2

"""

  def beforeAfter = {
    val messages = new ArrayBuffer[String]
    val spec = new Spec with BeforeAfterAll { sequential
      def is =
        s2""" e1 $e1
            | e2 $e2
            | """.stripMargin

      def e1 = { messages.append("e1"); ok }
      def e2 = { messages.append("e2"); ok }
      def beforeAll = messages.append("before all")
      def afterAll = messages.append("after all")
    }

    runSpec(spec)
    messages.toList ==== List("before all", "e1", "e2", "after all")
  }

  def withTags1 = {
    val messages = new ArrayBuffer[String]
    val spec = new Spec with BeforeAfterAll { sequential
      def is =
        s2"""
            | ${section("s")}
            | e1 $e1
            | ${section("s")}
            | e2 $e2
            | """.stripMargin

      def e1 = { messages.append("e1"); ok }
      def e2 = { messages.append("e2"); ok }
      def beforeAll = messages.append("before all")
      def afterAll = messages.append("after all")
    }

    runSpec(spec, arguments = Arguments("include", "s"))
    messages.toList ==== List("before all", "e1", "after all")
  }

  def withTags2 = {
    val messages = new ArrayBuffer[String]
    val spec = new Spec with BeforeAfterAll { sequential
      def is =
        s2"""
            | ${section("s")}
            | e1 $e1
            | ${section("s")}
            | e2 $e2
            | """.stripMargin

      def e1 = { messages.append("e1"); ok }
      def e2 = { messages.append("e2"); ok }
      def beforeAll = messages.append("before all")
      def afterAll = messages.append("after all")
    }

    runSpec(spec, arguments = Arguments("exclude", "s"))
    messages.toList ==== List("before all", "e2", "after all")
  }

  def runSpec(s: SpecificationStructure, arguments: Arguments = Arguments()) = {
    val env = Env(arguments = arguments, lineLogger = NoLineLogger)
    runAction(ClassRunner.report(env)(s), noLogging)
  }


}
