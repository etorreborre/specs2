package org.specs2
package reporter

import scala.collection.JavaConversions._
import org.junit.runner.Description
import org.specs2.internal.scalaz._
import Scalaz._
import Tree._

/**
 * Implementation of the Show trait to allow the drawing of Tree[Description] with
 * scalaz
 */
private[specs2]
trait ShowDescription {
  implicit object show extends Show[Description] {
    override def show(d: Description) = d.getDisplayName
  }
  implicit def toTree(desc: Description): Tree[Description] = 
	  unfoldTree(desc)((d: Description) => (d, () => d.getChildren.toStream))
}
private[specs2]
object ShowDescription extends ShowDescription
