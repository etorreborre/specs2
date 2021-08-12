package org.specs2
package control

object Throwables:
  def render(t: Throwable): String =
    s"${t.getClass.getName}" + (Option(t.getMessage) match {
      case Some(message) => s": $message"
      case _             => ""
    })

  def renderWithStack(t: Throwable): String =
    s"""============================================================
       |${render(t)}
       |------------------------------------------------------------
       |${traceWithIndent(t, "    ")}
       |============================================================
       |""".stripMargin

  def trace(t: Throwable): String =
    val out = new java.io.StringWriter
    t.printStackTrace(new java.io.PrintWriter(out))
    out.toString

  def traceWithIndent(t: Throwable, indent: String): String =
    // Predef.augmentString = work around scala/bug#11125 on JDK 11
    Predef.augmentString(trace(t)).linesIterator.map(line => indent + line).mkString("\n")
