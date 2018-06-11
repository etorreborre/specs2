package org.specs2
package html

import scala.xml.NodeSeq

trait HtmlDocuments {

  val aBodyWithHeaders: NodeSeq =
    <body>
      <h1>title</h1>text with
      <h2>a header</h2>
    </body>

  val aBodyWithTwoH3HeadersAndOneH4Each: NodeSeq =
    <h1>title</h1>             ++
      <h3>a h3 header</h3>     ++
      <h4>first h4</h4>        ++
      <h3>another h3 header</h3> ++
      <h4>second h4</h4>

  val aBodyWithAH3ThenAH2Header: NodeSeq =
    <h3>a h3 header</h3>     ++
      <h2>a h2 header</h2>

}