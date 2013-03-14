package org.specs2
package matcher
import execute._
import specification._
import java.util.regex.Pattern

class StringMatchersSpec extends Specification { def is = s2"""
  
  A string can be matched against a pattern using
    beMatching
    ${ "eric" must beMatching("e.*") }
    ${ "eric" must beMatching(Pattern.compile("e.*")) }
    ${ "eric" must beMatching("e.*".r) }

    "or 'be matching'
    ${ "eric" aka "ETO" must be matching("e.*") }
    ${ "a" must not be matching("{\"a\":\"b\"}") }

    find ... withGroups, to check for groups
    ${ "erirec" must find("(e|i).").withGroups("e", "i", "e") }
    ${ "abcd" must find("(a.)(c.)").withGroups("ab", "cd") }
    ${ "abcd" must find("(a.)(c.)".r).withGroups("ab", "cd") }

  The length of a string can be checked
    with have length
    ${ "Eric" must haveLength(4) }
    ${ "Eric" must have length(4) }
    ${ "Eric" must not have length(3) }

    or with haveSize because a String is also an Iterable[Char]
    ${ "Eric" must haveSize(4) }
    ${ "Eric" must have size(4) }
    ${ "Eric" must not have size(3) }

    or with beEmpty because a String is also an Iterable[Char]
    ${ "" must beEmpty }
    ${ "" must be empty }

  2 strings can be checked for equality
    ignoring case
    ${ "eric" must beEqualTo("Eric").ignoreCase }
    "the failure message must mention 'ignoring case' $e3

    ignoring space
    ${ "eric" must beEqualTo(" eric ").ignoreSpace }
    the failure message must mention 'ignoring space' $e4

    ignoring space and case
    ${ "  eric" must beEqualTo(" Eric ").ignoreSpace.ignoreCase }
    "the failure message must mention 'ignoring space, ignoring case' $e5

    alternatively, ignoring case and space
    ${ "Eric".aka must beEqualTo(" eric ").ignoreCase.ignoreSpace }

  It is possible to check if one string is contained in another one
  ${ "Eric" must contain("ri") }
  ${ "Eric" must not contain("ra") }

  It is possible to check if one string starts with another one
  ${ "Eric" must startWith("Er") }
  ${ "Eric" must not startWith("Eu") }

  It is possible to check if one string ends with another one
  ${ "Eric" must endWith("ic") }
  ${ "Eric" must not endWith("rac") }
                                                                                                                        """
  
  def e3 = ("eric".aka must beEqualTo("Xric").ignoreCase) returns "ignoring case"
  def e4 = ("eric".aka must beEqualTo("a eric ").ignoreSpace) returns "ignoring space"
  def e5 = ("eric".aka must beEqualTo("xric ").ignoreSpace.ignoreCase) returns
             "ignoring space, ignoring case"
  def e6 = "eric".aka must beEqualTo(" Eric ").ignoreCase.ignoreSpace
}