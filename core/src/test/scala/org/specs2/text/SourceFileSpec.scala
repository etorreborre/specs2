package org.specs2
package text

import matcher.TypedEqual
import specification.Grouped

class SourceFileSpec extends Spec with Grouped with SourceFile with TypedEqual { def is = sequential ^ s2"""

 the package name of a source file can be extracted
   for a simple name                                                     ${g1.e1}
   with a following semi-column                                          ${g1.e2}
   with several declarations                                             ${g1.e3}
   with a license header                                                 ${g1.e4}
                                                                         """


  "packages" - new g1 {
    e1 := packageName {
      """
      package test
      class HelloWorld
      """
    } === "test"

    e2 := packageName {
      """
      package test;
      class HelloWorld
      """
    } === "test"

    e3 := packageName {
      """
      package com

      package test
      package me

      class HelloWorld
      <package />
      """
    } === "com.test.me"

    e4 := packageName {
      """
      /** Copyright myself
       *  with no warranties of any sort
       */
      package com
      package test
      class HelloWorld
      """
    } === "com.test"
  }
}
