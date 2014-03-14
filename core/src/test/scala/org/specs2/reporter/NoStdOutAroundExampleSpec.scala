package org.specs2.reporter

import org.specs2.Specification

class NoStdOutAroundExampleSpec extends Specification with NoStdOutAroundExample { def is = s2"""

 In this test we need to check that there is no output in the console and that the regular
 standard output is set and the end of the test

  this has to be a visual test e5 $e5
  this has to be a visual test e4 $e4
  this has to be a visual test e3 $e3
  this has to be a visual test e2 $e2
  this has to be a visual test e1 $e1

  this has to be a visual test e5 $e5
  this has to be a visual test e4 $e4
  this has to be a visual test e3 $e3
  this has to be a visual test e2 $e2
  this has to be a visual test e1 $e1

  this has to be a visual test e5 $e5
  this has to be a visual test e4 $e4
  this has to be a visual test e3 $e3
  this has to be a visual test e2 $e2
  this has to be a visual test e1 $e1

  this has to be a visual test e5 $e5
  this has to be a visual test e4 $e4
  this has to be a visual test e3 $e3
  this has to be a visual test e2 $e2
  this has to be a visual test e1 $e1
"""

  def e1 = { Thread.sleep(10); "e1".pp; ok }
  def e2 = { Thread.sleep(20); "e2".pp; ok }
  def e3 = { Thread.sleep(30); "e2".pp; ok }
  def e4 = { Thread.sleep(40); "e2".pp; ok }
  def e5 = { Thread.sleep(50); "e2".pp; ok }

}
