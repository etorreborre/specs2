package org.specs2
package text
import CamelCase._

class CamelCaseSpec extends SpecificationWithJUnit { def is =

  "Camel case to words examples"                                                                                        ^
  { "HelloWorld".camelCaseToWords === "hello world" }                                                                   ^
  { "hello".camelCaseToWords === "hello" }                                                                              ^
  { "helloWorld".camelCaseToWords === "hello world" }                                                                   ^
  { "helloDearWorld".camelCaseToWords === "hello dear world" }                                                          ^
  { "".camelCaseToWords === "" }                                                                                        ^
                                                                                                                        end
}