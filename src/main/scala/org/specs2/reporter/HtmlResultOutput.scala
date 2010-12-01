package org.specs2
package reporter
import scala.xml._
import java.io.Writer
import main.Arguments

class HtmlResultOutput(out: Writer, val xml: NodeSeq = NodeSeq.Empty) {
  
  def printPar(text: String = "", doIt: Boolean = true)(implicit args: Arguments) = 
    if (doIt) printElem(<p>{text}</p>)
    else this

  def printTextPar(text: String = "", doIt: Boolean = true)(implicit args: Arguments) = 
    if (doIt) printElem(<p class="text">{text}</p>)
    else this

  def printSpecStart(message: String)(implicit args: Arguments): HtmlResultOutput = 
    printElem(<title>{message}</title>)
  

  def printSuccess(message: String, doIt: Boolean = true)(implicit args: Arguments) = 
    if (doIt) printElem(<div>{message}</div>)
    else this
  
  def printError(message: String, doIt: Boolean = true)(implicit args: Arguments) = 
    if (doIt) printElem(<div>{message}</div>)
    else this
  
  def printSkipped(message: String, doIt: Boolean = true)(implicit args: Arguments) = 
    if (doIt) printElem(<div>{message}</div>)
    else this

  def printPending(message: String, doIt: Boolean = true)(implicit args: Arguments) =
    if (doIt) printElem(<div>{message}</div>)
    else this 
    
  def printElem(xml2: Elem, doIt: Boolean = true)(implicit args: Arguments) = {
    if (doIt) new HtmlResultOutput(out, xml ++ xml2)
    else this
  }
  def printHead = new HtmlResultOutput(out, xml ++ head)
  def head = 
    <head>
        <style type="text/css" media="all">
          @import url('./css/maven-base.css');
          @import url('./css/maven-theme.css');
          @import url('./css/site.css');
        </style>
        <link href="./css/prettify.css" type="text/css" rel="stylesheet" />
        <script type="text/javascript" src="./css/prettify.js"></script>
        <link rel="stylesheet" href="./css/print.css" type="text/css" media="print" />
        <link href="./css/tooltip.css" rel="stylesheet" type="text/css" />
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
        <script type="text/javascript" src="./css/tooltip.js"/>
        <script language="javascript">window.onload={"init;"}</script>
        <!-- the tabber.js file must be loaded after the onload function has been set, in order to run the
             tabber code, then the init code -->
        <script type="text/javascript" src="./css/tabber.js"></script> 
        <link rel="stylesheet" href="./css/tabber.css" type="text/css" media="screen"/> 
    </head>
  
  def flush = {
    val toPrint = 
      if (xml.size == 1) new scala.xml.PrettyPrinter(10000, 2).format(xml(0))
      else if (xml.size > 1) new scala.xml.PrettyPrinter(10000, 2).format(Group(xml))
      else xml.toString
    out.write(toPrint)
  }
}
