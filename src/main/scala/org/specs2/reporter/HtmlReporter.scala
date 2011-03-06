package org.specs2
package reporter

import io.ConsoleOutput

/**
 * Reports a Specification as an Html page
 */
trait HtmlReporter extends Reporter
    with DefaultSelection
    with DefaultSequence
    with DefaultExecutionStrategy
    with HtmlExporting
    with ConsoleOutput 
