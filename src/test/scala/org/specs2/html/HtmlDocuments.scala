package org.specs2
package html

trait HtmlDocuments {
  val aBodyWithHeadersButNoToc           = <body><h1>title</h1>text with <h2>a header</h2></body>
  val aBodyWithNoHeaders                 = <body><h1>title</h1>text with <toc/><i>other nodes</i></body>
  val aBodyWithH1HeadersOnly             = <body><h1>title</h1>text with <toc/><h1>a h1 header</h1></body>
  val aBodyWithH2HeadersOnly             = <body><h1>title</h1>text with <toc/><h2>a h2 header</h2></body>
  val aBodyWithOneH3Header               = <body><h1>title</h1>text with <toc/><h3>a h3 header</h3></body>
  val aBodyWithTwoH3Headers              = <body><h1>title</h1>text with <toc/><h3>a h3 header</h3>other text<h3>another h3 header</h3></body>
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