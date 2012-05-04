package org.specs2
package html

import io.Paths._

/**
 * This class provides functionalities for manipulating Markdown links
 */
case class MarkdownLink(name: String, url: String) {
  def up = copy(url = "../" + url)

  override def toString = "[" + name + "](" + url.uriEncode + ")"
}
