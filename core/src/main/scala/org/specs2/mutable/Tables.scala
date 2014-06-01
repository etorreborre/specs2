package org.specs2
package mutable

import matcher.DataTables
import specification.dsl.NoBangExampleDsl

/**
 * This trait uses the NoBangExamples trait to allow using ! on string values in a DataTable
 */
trait Tables extends DataTables with NoBangExampleDsl
