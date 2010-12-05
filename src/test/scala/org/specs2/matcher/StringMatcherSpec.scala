package org.specs2
package matcher
import execute._
import specification._

class StringMatcherSpec extends SpecificationWithJUnit {  def is = 
  "A string can be matched against a pattern using"                                       ^
    "beMatching"                                                                          ^
    { "eric" must beMatching("e.*") }                                                     ^
                                                                                          p^
    "or 'be matching'"                                                                    ^
    { "eric" aka "ETO" must be matching("e.*") }                                          ^
                                                                                          endp^
  "2 strings can be checked for equality"                                                 ^  
    "ignoring case"                                                                       ^ 
    { "eric" must beEqualTo("Eric").ignoreCase }                                          ^  
      "the failure message must mention 'ignoring case'"                                  ! e3^
                                                                                          p^
    "ignoring space"                                                                      ^ 
    { "eric" must beEqualTo(" eric ").ignoreSpace }                                       ^
      "the failure message must mention 'ignoring space'"                                 ! e4^ 
                                                                                          p^
    "ignoring space and case"                                                             ^ 
    { "  eric" must beEqualTo(" Eric ").ignoreSpace.ignoreCase }                          ^
      "the failure message must mention 'ignoring space, ignoring case'"                  ! e5^ 
                                                                                          p^
    "alternately, ignoring case and space"                                                ^
    { "Eric".aka must beEqualTo(" eric ").ignoreCase.ignoreSpace }                        ^
                                                                                          endp^
  "It is possible to check if one string is contained in another one"                     ^
  { "Eric" must contain("ri") }                                                           ^
  { "Eric" must not contain("ra") }                                                       ^
                                                                                          endp^
  "It is possible to check if one string starts with another one"                         ^
  { "Eric" must startWith("Er") }                                                         ^
  { "Eric" must not startWith("Eu") }                                                ^
                                                                                          endp^
  "It is possible to check if one string ends with another one"                           ^
  { "Eric" must endWith("ic") }                                                           ^
  { "Eric" must not endWith("rac") }                                                 ^
                                                                                          end
  
  def e3 = ("eric".aka must beEqualTo("Xric").ignoreCase) returns "ignoring case"
  def e4 = ("eric".aka must beEqualTo("a eric ").ignoreSpace) returns "ignoring space"
  def e5 = ("eric".aka must beEqualTo("xric ").ignoreSpace.ignoreCase) returns 
             "ignoring space, ignoring case"
  def e6 = "eric".aka must beEqualTo(" Eric ").ignoreCase.ignoreSpace
}