package org.specs2
package text

import specification.Grouped

class SourceFileSpec extends Specification with Grouped with SourceFile { def is =

  "the package name of a source file can be extracted"  ^
    "for a simple name"            ! g1.e1^
    "with a following semi-column" ! g1.e2^
    "with several declarations"    ! g1.e3


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
  }
}
