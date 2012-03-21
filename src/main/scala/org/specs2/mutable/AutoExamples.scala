package org.specs2
package mutable

import text.Trim._

private [specs2]
trait AutoExamples extends org.specs2.specification.AutoExamples { this: specification.FragmentsBuilder =>

  override protected def getDescription(depth: Int = 12, startOffset: Int = -1, endOffset: Int = -1) =
    super.getDescription(depth, startOffset, endOffset)

  override private[specs2] def trimCode(code: String) = {
    List(";", "bt", "t", "endp", "br", "end", "p", ".", "eg", ".", ";").foldLeft(code.removeLast("\\(.*\\)"))(_.trim trimEnd _).
      trimFirst("eg").
      removeLast("\\(.*\\)").
      trimEnclosing("{", "}").
      trimEnclosing("`", "`").
      removeFirst("`\\(.*\\)").trimFirst("`")
  }

}
