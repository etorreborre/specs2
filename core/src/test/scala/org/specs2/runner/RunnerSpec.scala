package org.specs2
package runner

import main.Arguments
import control._
import Runner._

class RunnerSpec extends Specification { def is = s2"""

 A runner creates printers based on the command line arguments
   the console printer must be created if there aren't other printers $console1
   or if console is passed on the command line $console2

"""

  def console1 =
    createTextPrinter(Arguments.split(""), loader).runOption.flatten must beSome

  def console2 =
    createTextPrinter(Arguments.split("html console"), loader).runOption.flatten must beSome

  val loader = getClass.getClassLoader
}
