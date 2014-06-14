package org.specs2
package guide

import org.specs2.specification._

class TestAcceptanceSpec extends Specification with BeforeAfterEach with BeforeAfterAll
{ def is = sequential ^ s2"""
  first example $e1
  first example $e1
  first example $e1
  first example $e1
"""

  def beforeAll = "before all".pp
  def afterAll = "after all".pp

  def before = "before".pp
  def after = "after".pp

  def e1 = ok.pp


}
