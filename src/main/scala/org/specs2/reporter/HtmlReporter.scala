package org.specs2
package reporter

import io.ConsoleOutput

/**
 * Reports a Specification as an Html page
 */
private[specs2]
trait HtmlReporter extends Reporter 
    with DefaultSelection
    with DefaultExecutionStrategy
    with HtmlExporting
    with ConsoleOutput 
