package org.specs2
package mutable

import matcher.DataTables
import specification.NoBangExamples

/**
 * This trait uses the NoBangExamples trait to allow using ! on string values in a DataTable
 */
trait Tables extends DataTables with NoBangExamples
