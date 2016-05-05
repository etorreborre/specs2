package org.specs2
package reporter


/*
import mutable.Specification
import matcher.ResultMatchers
import scala.xml.NodeSeq
class HtmlUrlsSpecification extends Specification with ResultMatchers with HtmlUrls {
  skipAllIf(isDead("http://www.google.com"))

  "it is possible to check the web links of an html document" >> {
    check(<html><a href="http://www.google.com"></a></html>) must beSuccessful
    check(<html><a href="https://www.google.com"></a></html>) must beSuccessful
    check(<html><a href="http://speczzzz2.org"></a></html>) must beFailing("http://speczzzz2.org is dead")
  }
  "it is possible to check local links of an html document" >> {
    addFile("./index.html", "content")
    check(<html><a href="index.html"></a></html>) must beSuccessful
    check(<html><a href="index2.html"></a></html>) must beFailing("./index2.html is dead")
  }
  "it is possible to check local links of an html document, with anchors" >> {
    "in the same document" >> {
      check(<html><a href="#local"></a> <a name="local"/></html>) must beSuccessful
    }
    "in another document" >> {
      addFile("./guide.html", """<a name="spec+content"/>""")
      check(<html><a href="guide.html#spec+content"></a></html>) must beSuccessful
    }
  }
  "it is possible to check local links of an html document, with spaces" >> {
    addFile("./user guide.html", """<a name="spec+content"/>""")
    check(<html><a href="user%20guide.html"></a></html>) must beSuccessful
  }
  "it is possible to check local links of a relative html document" >> {
    check(<html><a href="../guide/price.html"></a></html>, Map("guide/price.html" -> NodeSeq.Empty), filePath = "guide/user.html") must beSuccessful
  }
  "only the failure messages are kept" >> {
    check {
      <html>
        <a href="http://www.google.com"></a>
        <a href="http://www.etorreborre1.com"></a>
        <a href="http://www.google.com"></a>
        <a href="http://www.etorreborre2.com"></a>
      </html>
    } must beFailing("http://www.etorreborre1.com is dead\nhttp://www.etorreborre2.com is dead")
  }
}
*/