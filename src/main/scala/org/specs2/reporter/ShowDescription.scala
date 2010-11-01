package org.specs2
package reporter

import scala.collection.JavaConversions._
import org.junit.runner.Description
import scalaz._
import Scalaz._

private[specs2]
trait ShowDescription {
  implicit object show extends scalaz.Show[Description] {
    def show(d: Description) = d.getDisplayName.toList
  }
  implicit def toTree(desc: Description): Tree[Description] = 
	  desc.unfoldTree((d: Description) => (d, () => d.getChildren.toStream))
}
private[specs2]
object ShowDescription extends ShowDescription
