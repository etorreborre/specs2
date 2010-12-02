package org.specs2
package reporter
import scala.xml._
import java.io.Writer
import main.Arguments
import execute.ResultStackTrace

class HtmlResultOutput(out: Writer, val xml: NodeSeq = NodeSeq.Empty) {
  
  def enclose(f: NodeSeq => NodeSeq)(rest: =>HtmlResultOutput)(implicit args: Arguments): HtmlResultOutput = {
    printNodeSeq(f(rest.xml))
  } 
  def printBr(doIt: Boolean = true)(implicit args: Arguments) = 
    if (doIt) printElem(<br></br>)
    else this
    
  def printPar(text: String = "", doIt: Boolean = true)(implicit args: Arguments) = 
    if (doIt) printElem(<p>{text}</p>)
    else this

  def printText(text: String = "", level: Int = 0, doIt: Boolean = true)(implicit args: Arguments) = 
    if (doIt) printElem(<div class={"level"+level}>{text}</div>)
    else this

  def printTextPar(text: String = "", level: Int = 0, doIt: Boolean = true)(implicit args: Arguments) = 
    if (doIt) printElem(<p class={"level"+level}>{text}</p>)
    else this

  def printSpecStart(message: String)(implicit args: Arguments): HtmlResultOutput = 
    printElem(<title>{message}</title>).
    printElem(<h2>{message}</h2>)
  
  def printWithIcon(message: String, iconName: String, level: Int = 0, doIt: Boolean = true)(implicit args: Arguments) = 
    if (doIt) printElem(<div class={"level"+level}><img src={icon(iconName)}/>{message}</div>)
    else this

  def printSuccess(message: String, level: Int = 0, doIt: Boolean = true)(implicit args: Arguments) = 
    printWithIcon(message, "success", level, doIt)
  
  def printFailure(message: String, level: Int = 0, doIt: Boolean = true)(implicit args: Arguments) = 
    printWithIcon(message, "failure", level, doIt)
    
  def printError(message: String, level: Int = 0, doIt: Boolean = true)(implicit args: Arguments) = 
    printWithIcon(message, "error", level, doIt)
  
  def printSkipped(message: String, level: Int = 0, doIt: Boolean = true)(implicit args: Arguments) = 
    printWithIcon(message, "skipped", level, doIt)

  def printPending(message: String, level: Int = 0, doIt: Boolean = true)(implicit args: Arguments) =
    printWithIcon(message, "info", level, doIt)
    
  def printStack(e: ResultStackTrace, level: Int, doIt: Boolean = true)(implicit args: Arguments) = {
    if (doIt) e.stackTrace.foldLeft(this) { (res, cur) => 
      res.printText(cur.toString, level)
    }
    else this
  }
  def printElem(xml2: Elem, doIt: Boolean = true)(implicit args: Arguments) = {
    if (doIt) new HtmlResultOutput(out, xml ++ xml2)
    else this
  }
  
  def printNodeSeq(xml2: NodeSeq, doIt: Boolean = true)(implicit args: Arguments) = {
    if (doIt) new HtmlResultOutput(out, xml ++ xml2)
    else this
  }

  def printHead = new HtmlResultOutput(out, xml ++ head)
  def head = 
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
        <style type="text/css" media="all">
          @import url('./css/maven-base.css');
          @import url('./css/maven-theme.css');
        </style>
        <link href="./css/prettify.css" type="text/css" rel="stylesheet" />
        <script type="text/javascript" src="./css/prettify.js"></script>
        <link rel="stylesheet" href="./css/print.css" type="text/css" media="print" />
        <link href="./css/tooltip.css" rel="stylesheet" type="text/css" />
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
  def icon(t: String) = "./images/icon_"+t+"_sml.gif"
}
