package org.specs2
package reporter

import main.Arguments
import control.StringOutputLogger
import matcher.ActionMatchers
import io.StringOutput
import specification.core.Env

class PrinterFactorySpec extends Specification with ActionMatchers {
  def is = s2"""

 A printer factory creates printers based on the command line arguments
   the console printer must be created if there aren't other printers $console1
   or if console is passed on the command line $console2

 If a printer can not be instantiated there must be
   a message                                         $instantiationFailure
   hints for specific printers
     if specs2-junit jar might be missing    $missingJunitJar
     if specs2-html jar might be missing     $missingHtmlJar

"""

  def console1 =
    val args = Arguments.split("")
    val factory = PrinterFactory.create(Env(args))
    factory.createTextPrinter.runOption.flatten must beSome

  def console2 =
    val args = Arguments.split("html console")
    val factory = PrinterFactory.create(Env(args))
    factory.createTextPrinter.runOption.flatten must beSome

  def instantiationFailure =
    createPrintersAndExpectMessage(Arguments.split("notifier missing"), "cannot create a missing notifier")

  def missingJunitJar =
    createPrintersAndExpectMessage(
      Arguments.split("junitxml"),
      "cannot create a JUnit XML printer. Please check that specs2-junit.jar"
    )

  def missingHtmlJar =
    createPrintersAndExpectMessage(
      Arguments.split("html"),
      "cannot create a HTML printer. Please check that specs2-html.jar"
    )

  def createPrintersAndExpectMessage(arguments: Arguments, message: String) =
    val output = new StringOutput {}
    val logger = StringOutputLogger(output)
    val factory = PrinterFactory.create(Env(arguments).setSystemLogger(logger))
    factory.createPrinters.runVoid
    output.messages must contain(contain(message))

  val loader = getClass.getClassLoader
}
