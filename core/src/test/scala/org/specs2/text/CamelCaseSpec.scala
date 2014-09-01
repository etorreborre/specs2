package org.specs2
package text
import CamelCase._

class CamelCaseSpec extends Spec { def is = s2"""

  CamelCased can be converted to space separated words
  ${ "HelloWorld".camelCaseToWords must_== "hello world" }
  ${ "hello".camelCaseToWords must_== "hello" }
  ${ "helloWorld".camelCaseToWords must_== "hello world" }
  ${ "helloDearWorld".camelCaseToWords must_== "hello dear world" }
  ${ "".camelCaseToWords must_== "" }
                                                                     """
}