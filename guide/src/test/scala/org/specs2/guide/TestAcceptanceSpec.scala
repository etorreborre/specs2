package org.specs2
package guide

import org.specs2.specification._

class TestAcceptanceSpec extends Specification { def is =  s2"""
  example 1 $e1
  example 2 $e1
  ${action {"stop here".pp}.stopOnError}
  example 3 $e1
  example 4 $e1
"""

  def beforeAll = "before all"
  def afterAll = "after all"

  def before = "before"
  def after = "after"

  def e1 = { name: String  => { name.pp; ok } }


}
