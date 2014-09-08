package org.specs2
package specification

import matcher.DataTables
import dsl.NoBangExamples

/**
 * This trait allows to use Data tables with simple ! as column separator
 * when the first column is a string. However the syntax for creating examples with a String
 * and a ! is deactivated
 */
trait Tables extends DataTables with NoBangExamples

