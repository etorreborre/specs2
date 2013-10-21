package org.specs2
package text
import CamelCase._

class CamelCaseSpec extends Specification { def is = s2"""

  CamelCased can be converted to space separated words
  ${ "HelloWorld".camelCaseToWords === "hello world" }
  ${ "hello".camelCaseToWords === "hello" }
  ${ "helloWorld".camelCaseToWords === "hello world" }
  ${ "helloDearWorld".camelCaseToWords === "hello dear world" }
  ${ "".camelCaseToWords === "" }
                                                                     """
}