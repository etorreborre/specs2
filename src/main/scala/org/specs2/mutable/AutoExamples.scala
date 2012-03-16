package org.specs2
package mutable

import text.Trim._

private [specs2]
trait AutoExamples extends org.specs2.specification.AutoExamples { this: specification.FragmentsBuilder =>
  override protected def getDescription(depth: Int = 12) = super.getDescription(12)

  override private[specs2] def trimCode(code: String) = {
    List(";", ".eg").foldLeft(code)(_.trim trimEnd _).
      trimEnclosing("{", "}").
      trimEnclosing("`", "`").
      removeFirst("`\\(.*\\)").trimFirst("`")
  }

}
