package org.specs2
package reporter
import org.junit.runner.Description
import scalaz._; import Scalaz._
import scala.collection.JavaConversions._
trait ShowDescription {
  implicit object show extends scalaz.Show[Description] {
    def show(d: Description) = d.getDisplayName.toList
  }
  implicit def toTree(desc: Description): Tree[Description] = 
	  desc.unfoldTree((d: Description) => (d, () => d.getChildren.toStream))
}
object ShowDescription extends ShowDescription
