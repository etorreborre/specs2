package org.specs2
package guide

object HtmlOutput extends UserGuidePage { def is = s2"""

$specs2 can be used to produce HTML documentation to either:

 - report the execution of specifications
 - provide a user guide like this one

If Markdown notation is used in the specification text and you have [Pandoc](http://johnmacfarlane.net/pandoc) installed on the command line then it will rendered in the final output.

### Create Html files

When you execute a specification with the `html` command-line argument an HTML report is created in the `target/specs2-reports` directory. It will show the status of examples (success, failure,...) and stack traces if there are any errors.

You can use the following arguments to change the HTML generation:

 Name                    | Default value                                 | Description
 ----------------------- | --------------------------------------------- | ------------------------------
 `html.outdir`           | `target/specs2-reports`                       | output directory
 `html.template`         | `target/specs2-reports/templates/specs2.html` | copied from the `resources/templates` directory
 `html.variables`        | `Map[String, String]()`                       | those variables will be replaced during template evaluation
 `html.nostats`          | `false`                                       | if true no stats are displayed


### Use Pandoc for Markdown

[Markdown](http://commonmark.org) text is supported if [Pandoc](http://johnmacfarlane.net/pandoc) is available on the command line.

 Name                    | Default value                                | Description
 ----------------------- | -------------------------------------------- | ------------------------------
 `pandoc.exec`           | `pandoc`                                     | path to the Pandoc executable
 `pandoc.inputformat`    | `markdown+pipe_tables`                       | pandoc arguments (see the Pandoc [user guide](http://johnmacfarlane.net/pandoc/README.html))
 `pandoc.outputformat`   | `html`                                       |

### Use a different template

You can change the overall structure of the html page for a specification by providing a different template with the `html.template` variable. When using your custom template the following variables will be replaced:

 Name                    | Description
 ----------------------- | ------------------------------
 `$$title$$`             | specification title
 `$$issues$$`            | true if there are issues in the specification
 `$$body$$`              | the specification body

You can also pass your own variables by passing a map `name1=value1,name2=value2,...` to the `html.variables` argument. Those variables can then be used in the template:

 - by enclosing them in `$$`: `$$myVariable$$`

 - by using the `if/else/endif` construct:
```
$$if(issues)$$
<h1>Failed! $$title$$
$$else$$
<h1>$$title$$</h1>
$$endif$$
```

### Use other CSS/Javascript files

Custom CSS and JavaScript files can be used without changing the template. In order to do this just put your own `specs2-user.css` file in `src/test/resources/css` or your own `specs2-user.js` file in `src/test/resources/javascript`.

## Create an index

Here's something you can do to automatically create an index page for your specifications: ${snippet{

import org.specs2._
import specification.core._
import runner.SpecificationsFinder._

class index extends Specification { def is =

  examplesLinks("Example specifications")

  // see the SpecificationsFinder trait for the parameters of the 'specifications' method
  def examplesLinks(t: String) =
    t.title ^ specifications().map(s => link(s))
}
}}

The specification above creates an index.html file in the `target/specs2-reports` directory. The specifications method creates specifications using the following parameters:

 Name                    | Default                                       | Description
 ----------------------- | --------------------------------------------  | ----------------
 `glob`                  | `**/*.scala`                                  | glob pattern to filter specification files
 `pattern`               | `.*Spec`                                      | pattern to use when trying to retrieve the specification names from the source files
 `filter`                | `(name: String) => true`                      | function to keep only some specifications depending on their name
 `basePath`              | `src/test/scala`                              | the path where to start the search
 `verbose`               | `false`                                       | boolean indicating if information about finding files and specifications must be printed
 `classLoader`           | `Thread.currentThread.getContextClassLoader`  | classloader used to load the specification classes
 `filePathReader`        | `org.specs2.io.FileSystem`                    | object used to read source files


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
