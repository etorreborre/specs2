package org.specs2
package guide

import org.specs2.specification._

class TestAcceptanceSpec extends Specification with BeforeAfterAll { def is =  s2"""
  first example $e1
  first example $e1
  ${step { sys.error("bang");"stop here".pp}.stopOnError}
  first example $e1
  first example $e1
"""

  def beforeAll = "before all"
  def afterAll = "after all"

  def before = "before"
  def after = "after"

  def e1 = ok


}
