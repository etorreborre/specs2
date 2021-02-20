package org.specs2
package reporter

import org.junit.runner.Description

import org.specs2.fp._, Tree._
import scala.collection.JavaConverters._

/**
 * Implementation of the Show trait to allow the drawing of Tree[Description]
 */
trait ShowDescription:

  given Show[Description] with
    def show(d: Description): String =
      d.getDisplayName


  extension (desc: Description)
    def toTree: Tree[Description] =
      unfoldTree(desc)((d: Description) => (d, () => d.getChildren.asScala.to(LazyList)))


object ShowDescription extends ShowDescription
