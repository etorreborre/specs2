package org.specs2
package reporter

import ExecutedSpecificationData._

class HtmlLinesSpec extends Specification { def is = s2"""
                                                                                                                                           
The HtmlPrinter can reduce executed fragments to HtmlLine objects. Each fragment creates a specific HtmlLine,
with the applicable level, arguments and Statistics.
                                                                                                                                            
  An ExecutedSpecStart is translated to a HtmlSpecStart                                                $e1
  An ExecutedSpecEnd is translated to a HtmlSpecEnd                                                    $e2
  An ExecutedText is translated to a HtmlText                                                          $e3
  An ExecutedResult is translated to a HtmlResult                                                      $e4
  An ExecutedBr is translated to a HtmlBr                                                              $e5
  Everything else is translated to HtmlOther, meaning that those fragments will not be printed out     $e6
                                                                                                       """

  lazy val spec = execute(formatSection(flow=true) ^  "text" ^ { 1 === 1 } ^ br ^ t(2) ^ end)
  lazy val lines = printer.reduce(spec)

  def e1 = lines.head must haveClass[HtmlSpecStart]
  def e2 = lines.last must haveClass[HtmlSpecEnd]
  def e3 = lines(2) must haveClass[HtmlText]
  def e4 = lines(3) must haveClass[HtmlResult]
  def e5 = lines(4) must haveClass[HtmlBr]
  def e6 = lines(5) must haveClass[HtmlOther]

  lazy val printer = new HtmlPrinter {}
}