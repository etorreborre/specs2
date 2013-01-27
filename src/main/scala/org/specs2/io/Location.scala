package org.specs2
package io

/**
 * Location of a Fragment in a file.
 *
 * This class behavior is highly dependent on method calls in the FragmentsBuilder trait.
 *
 * Depending on the method that's called to create a fragment or to "link" it to another one, we have to do ad-hoc stacktrace
 * manipulations to determine the exact location of a Fragment.
 */
class Location {
  private val location = FromSource.location(fragmentFilter)
  def file: String = location.fileName
  def lineNumber: Int = location.lineNumber
  override def toString = location.fullLocation
  override def equals(a: Any) = a match {
    case l: Location => l.toString == this.toString
    case other       => false
  }

  private def isFragmentDefinition(s: String) =
    Seq("Described",
        "InExample"
  ).map("org.specs2.mutable.FragmentsBuilder$"+_).exists(s.contains)

  private def fragmentFilter = (st: Seq[StackTraceElement]) => {
    // for a mutable specification we drop the stacktrace until there are fragment
    // definitions then we drop the definition calls
    if (isMutableSpecification(st))
      st.dropWhile(s => !isFragmentDefinition(s.toString)).
         dropWhile(s => isFragmentDefinition(s.toString))
    // for an acceptance specification we have to drop some stacktrace element and adjust the line number of the method calls
    // to find out the exact location of a fragment
    else {
      val fragmentsMethods = Seq("org.specs2", ".textStart", ".textFragment", ".fragmentsFragments", ".p(", ".br(", ".t(", ".bt(", ".end(", ".endp(", ".endbr(")
      val (start, end) = st.span(s => fragmentsMethods.exists(s.toString.contains))
      // 154 is the magic line number of the "FragmentsFragment::def ^(t: String) = fs add Text(t)" method
      if (lastContains(start, "$up(FragmentsBuilder.scala:155") && !isTextStart(start)) takeHead(end, st(0), lineOffset = 1)
      else if (lastContains(start, "$up"))                                              takeHead(end, st(0))
      else                                                                              end
    }
  }

  private def isTextStart(st: Seq[StackTraceElement]) = st.exists(_.toString.contains(".textStart"))
  private def lastContains(st: Seq[StackTraceElement], method: String) = st.lastOption.map(_.toString.contains(method)).getOrElse(false)
  private def takeHead(st: Seq[StackTraceElement], defaultValue: StackTraceElement, lineOffset: Int = 0) = {
    val stElement = st.headOption.getOrElse(defaultValue)
    Seq(new StackTraceElement(stElement.getClassName, stElement.getMethodName, stElement.getFileName, stElement.getLineNumber+lineOffset))
  }

  private def isMutableSpecification(st: Seq[StackTraceElement]) = st.exists(_.toString.contains("org.specs2.mutable.FragmentsBuilder"))
}
