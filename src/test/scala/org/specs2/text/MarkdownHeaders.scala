package org.specs2
package text

/**
 * usage: h4> "This is a h4 title"
 */
trait MarkdownHeaders {
  lazy val (h1, h2, h3, h4, h5, h6) = (MarkdownHeader(1), MarkdownHeader(2), MarkdownHeader(3), MarkdownHeader(4), MarkdownHeader(5), MarkdownHeader(6))

  case class MarkdownHeader(level: Int) {
    def >(s: String) = ("#" * level) + " " + s
  }
}
object MarkdownHeaders extends MarkdownHeaders