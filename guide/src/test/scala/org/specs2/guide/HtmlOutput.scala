package org.specs2
package guide

object HtmlOutput extends UserGuidePage { def is = s2"""

$specs2 can be used to produce HTML documentation to either:

 - report the execution of specifications
 - provide a user guide like this one

If Markdown notation is used in the specification text and you have [Pandoc](http://johnmacfarlane.net/pandoc) installed then it will rendered in the final output.

### Create Html files

When you execute a specification with the `html` argument an HTML report is created in the `target/specs2-reports` directory. It will show the status of examples (success, failure,...) and stack traces if there are any errors.

You can use the following arguments to change the HTML generation:

 Name                    | Default value                                 | Description
 ----------------------- | --------------------------------------------- | ------------------------------
 `html.outdir`           | `target/specs2-reports`                       | output directory
 `html.template`         | `target/specs2-reports/templates/specs2.html` | copied from the `resources/templates` directory
 `html.variables`        | `Map[String, String]()`                       | those variables will be replace during template evaluation
 `html.nostats`          | `false`                                       | if true no stats are displayed


### Use Pandoc for Markdown

[Markdown](http://commonmark.org) text is supported if [Pandoc](http://johnmacfarlane.net/pandoc) is available on the command line.

 Name                    | Default value                                | Description
 ----------------------- | -------------------------------------------- | ------------------------------
 `pandoc.exec`           | `pandoc`                                     | path to the Pandoc executable
 `pandoc.inputformat`    | `markdown+pipe_tables`                       | pandoc arguments (see the Pandoc [user guide](http://johnmacfarlane.net/pandoc/README.html))
 `pandoc.outputformat`   | `html`                                       |

### Use a different template

### Use other CSS/Javascript files

## Create an index

## Arguments



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
