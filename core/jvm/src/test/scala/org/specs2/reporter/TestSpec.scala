package org.specs2.reporter

import org.specs2.*
import specification.*
import execute.*
import main.*

class TestSpec extends Specification with Tables:
  def is = s2"""

  The results of a specification can be printed as lines

  example1 $ok
  the title of the specification must be printed first $ok
  the title is not displayed if # can not be shown $ok
  regular text must have no status $ok ${tag("xxx")}
  example2 $ok
  bbooo $ok




  example3 $ko
  example3 $ok ${tag("xxx")}
  example4 $ok ${tag("xxx")}

  example4 $ko

  example5 $ok
  example6 $ok ${tag("xxx")}
  example7 $ko
  baa $ok

  """

  def error1 = { throw new Exception("boom"); ok }
