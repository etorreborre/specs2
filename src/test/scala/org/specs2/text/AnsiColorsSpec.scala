package org.specs2
package text

class AnsiColorsSpec extends SpecificationWithJUnit {
  def is = 
"  it is possible to remove the colors from a string"    ! e1^
                                                         end

  def e1 = AnsiColors.removeColors("hello" + AnsiColors.red) must_== "hello"  
}