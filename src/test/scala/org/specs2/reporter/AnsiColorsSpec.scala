package org.specs2
package reporter

class AnsiColorsSpec extends Specification {
  def content = 
"  it is possible to remove the colors from a string"    ! e1^
                                                         end

  def e1 = AnsiColors.removeColors("hello" + AnsiColors.red) must_== "hello"  
}