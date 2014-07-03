package org.specs2
package guide

object HtmlOutput extends UserGuidePage { def is = s2"""

The `html` argument creates an HTML report for the specification, located in the `target/specs2-reports` directory by default.

Pandoc for markdown
Template
Custom CSS
Custom Javascript


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

