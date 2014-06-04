package org.specs2
package runner

import control._
import Actions._
import reflect.Classes
import specification.core._
import reporter._
import main.Arguments
import reporter.Printer._
import scalaz.std.anyVal._
import scalaz.syntax.bind._
import scalaz.syntax.traverse._
import scalaz.std.list._
import Runner._

/**
 * The class runner expects the first argument to be the class name of
 * a specification to execute
 */
trait ClassRunner {
  /**
   * run the specification
   */
  def run(args: Array[String]) {
    val env = Env(arguments = Arguments(args.drop(1).mkString(" ")))

    val actions: Action[Unit] = args.toList match {
      case Nil =>
        Actions.fail("there must be at least one argument, the fully qualified class name")

      case className :: rest =>
        createSpecification(className).flatMap(report(env)) >>
        Actions.safe(env.shutdown)
    }
    execute(actions)
  }

  /** create the specification from the class name */
  def createSpecification(className: String): Action[SpecificationStructure] =
    Classes.createInstance[SpecificationStructure](className, Thread.currentThread.getContextClassLoader)

  /** report the specification */
  def report(env: Env) = { spec: SpecificationStructure =>
    val loader = Thread.currentThread.getContextClassLoader

    if (env.arguments.commandLine.contains("all")) {
      for {
        printers <- createPrinters(env.arguments, loader)
        ss       <- SpecificationStructure.linkedSpecifications(spec, env, loader)
        sorted   <- safe(SpecificationStructure.topologicalSort(env)(ss).getOrElse(Seq()) :+ spec)
        rs = sorted.toList.map(s => Reporter.report(env, printers)(s.structure(env))).sequenceU
      } yield rs

    } else
      createPrinters(env.arguments, loader).map(printers => Reporter.report(env, printers)(spec.structure(env)))
  }

  /** accepted printers */
  def createPrinters(args: Arguments, loader: ClassLoader): Action[List[Printer]] =
    List(createTextPrinter(args, loader),
         createJUnitXmlPrinter(args, loader),
         createHtmlPrinter(args, loader),
         createMarkdownPrinter(args, loader),
         createPrinter(args, loader),
         createNotifierPrinter(args, loader)).sequenceU.map(_.flatten)

}

object ClassRunner extends ClassRunner

object consoleRunner extends ClassRunner {
  def main(args: Array[String]) = run(args)
}

