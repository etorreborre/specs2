package org.specs2
package reporter

import org.junit.runner.Description

import org.specs2.fp._, Tree._
import scala.collection.JavaConverters._

/**
 * Implementation of the Show trait to allow the drawing of Tree[Description]
 */
trait ShowDescription {

  implicit object show extends Show[Description] {
    def show(d: Description) = d.getDisplayName
  }

  implicit def toTree(desc: Description): Tree[Description] =
    unfoldTree(desc)((d: Description) => (d, () => d.getChildren.asScala.toStream))

}

object ShowDescription extends ShowDescription

