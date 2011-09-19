package org.specs2
package reporter

import mock._
import specification._
import text.MarkdownHeaders._

class HtmlLinesSpec extends Specification with Mockito { def is =
                                                                                                                        """
The HtmlPrinter can reduce executed fragments to HtmlLine objects containing:

 * an `Html` object                                                                                                                        """
                                                                                                                        p^
  h4> "HtmlSpecStart"                                                                                                   ^
                                                                                                                        end



}