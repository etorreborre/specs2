package org.specs2
package text

import java.util.regex.Pattern

import matcher.TypedEqual
import specification.Grouped
import control._

class SourceFileSpec extends Spec with Grouped with TypedEqual { def is = sequential ^ s2"""

 the package name of a source file can be extracted
   for a simple name                                                     ${g1.e1}
   with a following semi-column                                          ${g1.e2}
   with several declarations                                             ${g1.e3}
   with a license header                                                 ${g1.e4}
   with a comment at the end of the line                                 ${g1.e5}

 class names can be found
   for a non-empty package                                               ${g2.e1}
   for an empty package                                                  ${g2.e2}

                                                                         """

  val sourceFile = SourceFile(NoLogger)

  "packages" - new g1 {
    e1 := sourceFile.packageName {
      """
      package test
      class HelloWorld
      """
    } === "test"

    e2 := sourceFile.packageName {
      """
      package test;
      class HelloWorld
      """
    } === "test"

    e3 := sourceFile.packageName {
      """
      package com

      package test
      package me

      class HelloWorld
      <package />
      """
    } === "com.test.me"

    e4 := sourceFile.packageName {
      """
      /** Copyright myself
       *  with no warranties of any sort
       */
      package com
      package test
      class HelloWorld
      """
    } === "com.test"

    e5 := sourceFile.packageName {
      """
      package com//the com package
      package test // the test package
      class HelloWorld
      """
    } === "com.test"
  }

  "class names" - new g2 {
    val pattern = Pattern.compile("\\s*class\\s*(.*Spec)\\s*extends\\s*.*")
    val content = "\nclass MySpec extends Spec\n"
    e1 := sourceFile.classNames("com.example", content, pattern, suffix = "", verbose = true).runOption must beSome(Seq("com.example.MySpec"))

    e2 := sourceFile.classNames("", content, pattern, suffix = "", verbose = true).runOption must beSome(Seq("MySpec"))

  }
}
