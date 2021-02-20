package org.specs2
package text

import java.util.regex.Pattern

import matcher.TypedEqual
import control.*

class SourceFileSpec extends Spec with TypedEqual { def is = sequential ^ s2"""

 the package name of a source file can be extracted
   for a simple name                                                     $packages1
   with a following semi-column                                          $packages2
   with several declarations                                             $packages3
   with a license header                                                 $packages4
   with a comment at the end of the line                                 $packages5

 class names can be found
   for a non-empty package                                               $classNames1
   for an empty package                                                  $classNames2

"""

  val sourceFile = SourceFile(NoLogger)

  def packages1 = sourceFile.packageName {
    """
    package test
    class HelloWorld
    """
  } === "test"

  def packages2 = sourceFile.packageName {
    """
    package test;
    class HelloWorld
    """
  } === "test"

  def packages3 = sourceFile.packageName {
    """
    package com
    package test
    package me
    class HelloWorld
    <package />
    """
  } === "com.test.me"

  def packages4 = sourceFile.packageName {
    """
    /** Copyright myself
     *  with no warranties of any sort
     */
    package com
    package test
    class HelloWorld
    """
  } === "com.test"

  def packages5 = sourceFile.packageName {
    """
    package com//the com package
    package test // the test package
    class HelloWorld
    """
  } === "com.test"

  val pattern = Pattern.compile("\\s*class\\s*(.*Spec)\\s*extends\\s*.*")
  val content = "\nclass MySpec extends Spec\n"

  def classNames1 =
    sourceFile.classNames("com.example", content, pattern, suffix = "", verbose = true).runOption `must` beSome(Seq("com.example.MySpec"))

  def classNames2 =
    sourceFile.classNames("", content, pattern, suffix = "", verbose = true).runOption `must` beSome(Seq("MySpec"))

}
