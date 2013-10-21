package org.specs2
package html

trait HtmlDocuments {

  val aBodyWithHeaders           =
    <body>
      <h1>title</h1>text with
      <h2>a header</h2>
    </body>

  val aBodyWithHeadersAndASubtoc =
    <body>
      <h1>title</h1>
      <h3>a h3 header</h3>
      <subtoc specId="123"/>
      <h4>first h4</h4>
      <h3>another h3 header</h3>
      <h4>second h4</h4>
    </body>

  val aBodyWithTwoH3HeadersAndOneH4Each  =
    <h1>title</h1>             ++
      <h3>a h3 header</h3>     ++
      <h4>first h4</h4>        ++
      <h3>another h3 header</h3> ++
      <h4>second h4</h4>

  val aBodyWithAH3ThenAH2Header  =
    <h3>a h3 header</h3>     ++
      <h2>a h2 header</h2>

}