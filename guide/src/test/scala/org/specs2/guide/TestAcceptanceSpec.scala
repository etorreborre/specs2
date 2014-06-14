package org.specs2
package guide

class TestAcceptanceSpec extends Specification { def is = s2"""
  first example $e1
"""

  def e1 = ko("message")


}
