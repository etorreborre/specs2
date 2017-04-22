package org.specs2
package reporter

import io.DirectoryPath
import main.Arguments
import specification.process.Level
import specification.core.Fragment

object HtmlBodyPrinterSpec extends Specification { def is = s2"""

 A hidden reference must not be printed $hidden

"""

  def hidden = {
    print(link(HtmlBodyPrinterSpec).hide) must beEmpty
  }

  def print(f: Fragment) =
    HtmlBodyPrinter.printFragment(f, success, Arguments(), Level.Root, DirectoryPath.Root, pandoc = true)
}
