package org.specs2
package guide

object HtmlOutput extends UserGuidePage { def is = s2"""

The `html` argument creates an HTML report for the specification, located in the `target/specs2-reports` directory by default.

Pandoc for markdown
Template
Custom CSS
Custom Javascript

## Create an index

## Arguments

 Name                    | Default value                                | Description
 ----------------------- | -------------------------------------------- | ------------------------------
 `html.outdir`           | `target/specs2-reports/`                     |
 `html.basedir`          | `.`                                          |
 `html.template`         | `target/specs2-reports/templates/specs2.html | copied from the `resources/templates` directory
 `html.variables`        | `Map[String, String]()`                      | passed to the template during the Pandoc evaluation
 `html.nostats`          | `false`                                      | if true no stats are displayed

"""
}

class HtmlExampleSpec extends Specification { def is = s2"""

 This is a specification to check the 'Hello world' string

 The 'Hello world' string should
   contain 11 characters                                         $e1
   start with 'Hello'                                            $e2
   end with 'world'                                              $e3
                                                                 """

  def e1 = "Hello world" must haveSize(11)
  def e2 = "Hello world" must startWith("Hello")
  def e3 = "Hello world" must endWith("world")
}
