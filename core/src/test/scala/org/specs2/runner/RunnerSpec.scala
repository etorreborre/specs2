package org.specs2
package runner

import main.Arguments
import control._
import Runner._
import matcher.ActionMatchers
import org.specs2.io.StringOutput

class RunnerSpec extends Specification with ActionMatchers { def is = s2"""

 A runner creates printers based on the command line arguments
   the console printer must be created if there aren't other printers $console1
   or if console is passed on the command line $console2

 A runner instantiates specifications from names
   from object names            $objects1
   from object names without $$ $objects2
   from classes                 $classes

 If a printer can not be instantiated there must be
   a message                                         $instantiationFailure
   hints for specific printers
     if specs2-junit jar might be missing    $missingJunitJar
     if specs2-html jar might be missing     $missingHtmlJar

"""

  def console1 =
    createTextPrinter(Arguments.split(""), loader).runOption.flatten must beSome

  def console2 =
    createTextPrinter(Arguments.split("html console"), loader).runOption.flatten must beSome

  def objects1 =
    TextRunner.createSpecification("org.specs2.runner.RunnerSpecification$").runOption must beSome

  def objects2 =
    TextRunner.createSpecification("org.specs2.runner.RunnerSpecification").runOption must beSome

  def classes =
    TextRunner.createSpecification("org.specs2.runner.RunnerSpec").runOption must beSome

  def instantiationFailure =
    createPrintersAndExpectMessage(Arguments.split("notifier missing"), "cannot create a missing notifier")

  def missingJunitJar =
    createPrintersAndExpectMessage(Arguments.split("junitxml"), "cannot create a JUnit XML printer. Please check that specs2-junit.jar")

  def missingHtmlJar =
    createPrintersAndExpectMessage(Arguments.split("html"), "cannot create a HTML printer. Please check that specs2-html.jar")

  def createPrintersAndExpectMessage(arguments: Arguments, message: String) = {
    val output = new StringOutput {}
    val consoleLogger = (s: String) => output.println(s)
    runOperation(ClassRunner.createPrinters(arguments, loader), consoleLogger)
    output.messages must contain(contain(message))
  }

  val loader = getClass.getClassLoader
}

object RunnerSpecification extends Specification { def is = success }
