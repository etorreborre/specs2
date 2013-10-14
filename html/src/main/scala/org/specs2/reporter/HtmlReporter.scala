package org.specs2
package reporter

import io.ConsoleOutput

/**
 * Reports a Specification as an Html page
 */
trait HtmlReporter extends DefaultReporter
    with HtmlExporting
    with ConsoleOutput

/**
 * Reports a Specification as a markdown page
 */
trait MarkdownReporter extends DefaultReporter
with MarkdownExporting
with ConsoleOutput
