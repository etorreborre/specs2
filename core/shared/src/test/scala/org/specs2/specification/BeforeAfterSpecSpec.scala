package org.specs2
package specification

import main.Arguments
import reporter._
import reporter.PrinterLogger._
import specification.core.{SpecificationStructure, Env}
import scala.collection.mutable.ArrayBuffer

class BeforeAfterSpecSpec extends Specification { def is = s2"""

 Before and after all steps can be executed with the BeforeAfterSpec trait $beforeAfter ${tag("travis")}
 Before and after all steps can be executed even if tags are included     $withTags1
 Before and after all steps can be executed even if tags are excluded     $withTags2

"""

  def beforeAfter =
    val messages = new ArrayBuffer[String]
    val spec = new Spec with BeforeAfterSpec {
      def is = sequential ^
        s2""" e1 $e1
            | e2 $e2
            | """.stripMargin

      def e1 = { messages.append("e1"); ok }
      def e2 = { messages.append("e2"); ok }
      def beforeSpec = step { messages.append("before all"); ok }
      def afterSpec = step { messages.append("after all"); ok }
    }

    runSpec(spec)
    messages.toList ==== List("before all", "e1", "e2", "after all")

  def withTags1 =
    val messages = new ArrayBuffer[String]
    val spec = new Spec with BeforeAfterSpec {
      def is =
        sequential ^ s2"""
            | ${section("s")}
            | e1 $e1
            | ${section("s")}
            | e2 $e2
            | """.stripMargin

      def e1 = { messages.append("e1"); ok }
      def e2 = { messages.append("e2"); ok }

      def beforeSpec = step { messages.append("before all"); ok }
      def afterSpec = step { messages.append("after all"); ok }
    }

    runSpec(spec, arguments = Arguments("include", "s"))
    messages.toList ==== List("before all", "e1", "after all")

  def withTags2 =
    val messages = new ArrayBuffer[String]
    val spec = new Spec with BeforeAfterSpec {
      def is =
        sequential ^ s2"""
            | ${section("s")}
            | e1 $e1
            | ${section("s")}
            | e2 $e2
            | """.stripMargin

      def e1 = { messages.append("e1"); ok }
      def e2 = { messages.append("e2"); ok }
      def beforeSpec = step { messages.append("before all"); ok }
      def afterSpec = step { messages.append("after all"); ok }
    }

    runSpec(spec, arguments = Arguments("exclude", "s"))
    messages.toList ==== List("before all", "e2", "after all")

  def runSpec(s: SpecificationStructure, arguments: Arguments = Arguments()) =
    val env = Env(arguments = arguments, printerLogger = NoPrinterLogger)
    try Reporter.create(Nil, env).report(s.structure(env)).runVoid(env.executionEnv)
    finally env.shutdown()


}
