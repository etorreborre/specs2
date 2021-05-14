package org.specs2
package text
import CamelCase.*

class CamelCaseSpec extends Spec { def is = s2"""

  CamelCased can be converted to space separated words
  ${ "HelloWorld".camelCaseToWords must ===("hello world") }
  ${ "hello".camelCaseToWords must ===("hello") }
  ${ "helloWorld".camelCaseToWords must ===("hello world") }
  ${ "helloDearWorld".camelCaseToWords must ===("hello dear world") }
  ${ "".camelCaseToWords must ===("") }

"""
}
