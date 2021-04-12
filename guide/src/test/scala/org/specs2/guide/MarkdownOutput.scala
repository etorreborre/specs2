package org.specs2
package guide

object MarkdownOutput extends UserGuidePage { def is = s2"""

### Markdown printer

There is a simple printer for creating Markdown files based on your specifications. You simply invoke it by passing the `markdown` argument on the command line.

### Arguments

The following arguments are available to fine-tune the generation of Markdown pages

 Name                     | Default                         | Description
 ------------------------ | ------------------------------- | -----------
 `markdown.outdir`        | `target/specs2-reports`         | output directory for Markdown files
 `markdown.ext`           | `md`                            | extension for Markdown files

"""
}

