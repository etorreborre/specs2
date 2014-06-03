package org.specs2
package reporter

import org.junit.runner.Description

import scala.collection.JavaConversions._
import scalaz.Tree._
import scalaz.{Show, Tree}

/**
 * Implementation of the Show trait to allow the drawing of Tree[Description] with
 * scalaz
 */

trait ShowDescription {
  implicit object show extends Show[Description] {
    override def show(d: Description) = d.getDisplayName
  }
  implicit def toTree(desc: Description): Tree[Description] =
    unfoldTree(desc)((d: Description) => (d, () => d.getChildren.toStream))
}

object ShowDescription extends ShowDescription

