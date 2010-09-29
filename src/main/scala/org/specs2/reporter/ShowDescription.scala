package org.specs2
package reporter
import org.junit.runner.Description

trait ShowDescription {
  implicit object show extends scalaz.Show[Description] {
    def show(d: Description) = d.getDisplayName.toList
  }
}
object ShowDescription extends ShowDescription
