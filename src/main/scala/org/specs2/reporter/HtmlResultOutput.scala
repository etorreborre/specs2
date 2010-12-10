package org.specs2
package reporter
import scala.io.Source
import scala.xml._
import parsing.XhtmlParser
import java.io.Writer
import main.Arguments
import text.Markdown._
import text._
import execute.{ Result, ResultStackTrace }

/**
 * This class stores the html to print to a file (as a NodeSeq object)
 * 
 * An instance of that class is immutable so each print method returns another instance
 * containing more xml to print.
 *
 */
private[specs2]
class HtmlResultOutput(out: Writer, val xml: NodeSeq = NodeSeq.Empty) {
  
  /**
   * Usage: out.enclose((t: NodeSeq) => <body>{t}</body>)(<div>inside</div>))
   * 
   * to create <body><div>inside</div></body>)
   * 
   * @return some xml (rest) enclosed in another block
   */
  def enclose(f: NodeSeq => NodeSeq)(rest: =>HtmlResultOutput)(implicit args: Arguments): HtmlResultOutput = {
    printNodeSeq(f(rest.xml))
  } 
  private[specs2] lazy val blank = new HtmlResultOutput(out)
  
  def printBr(doIt: Boolean = true)(implicit args: Arguments) = 
    if (doIt) printElem(<br></br>)
    else this
    
  def printPar(text: String = "", doIt: Boolean = true)(implicit args: Arguments) = 
    if (doIt) printElem(<p>{wiki(text)}</p>)
    else this

  def printText(text: String = "", level: Int = 0, doIt: Boolean = true)(implicit args: Arguments) = 
    if (doIt) printElem(<div class={"level"+level}>{wiki(text)}</div>)
    else this

  def printTextPar(text: String = "", level: Int = 0, doIt: Boolean = true)(implicit args: Arguments) = 
    if (doIt) printElem(<p class={"level"+level}>{wiki(text)}</p>)
    else this

  def printSpecStart(message: String)(implicit args: Arguments): HtmlResultOutput = 
    printElem(<title>{wiki(message)}</title>).
    printElem(<h2>{wiki(message)}</h2>)
  
  def wiki(text: String) = XhtmlParser(Source.fromString("<text>"+toHtmlNoPar(text)+"</text>")).head.child
  
  def printWithIcon(message: MarkupString, iconName: String, level: Int = 0, doIt: Boolean = true)(implicit args: Arguments) =
    if (doIt) printElem(<div class={"level"+level}><img src={icon(iconName)}/>{wiki(message.toHtml)}</div>)
    else this
    
  def icon(t: String) = "./images/icon_"+t+"_sml.gif"

  def printSuccess(message: MarkupString, level: Int = 0, doIt: Boolean = true)(implicit args: Arguments) =
    printWithIcon(message, "success", level, doIt)
  
  def printFailure(message: MarkupString, level: Int = 0, doIt: Boolean = true)(implicit args: Arguments) =
    printWithIcon(message, "failure", level, doIt)
    
  def printError(message: MarkupString, level: Int = 0, doIt: Boolean = true)(implicit args: Arguments) =
    printWithIcon(message, "error", level, doIt)
  
  def printSkipped(message: MarkupString, level: Int = 0, doIt: Boolean = true)(implicit args: Arguments): HtmlResultOutput =
    printWithIcon(message, "skipped", level, doIt)

  def printPending(message: MarkupString, level: Int = 0, doIt: Boolean = true)(implicit args: Arguments) =
    printWithIcon(message, "info", level, doIt)
    
  def printExceptionMessage(e: Result with ResultStackTrace, level: Int, doIt: Boolean = true)(implicit args: Arguments) = {
    if (doIt) {
      val message = "  "+e.message+" ("+e.location+")"
      printElem(<div class={"level"+level}>{message}</div>)
    } else this
  }
  
  def printCollapsibleExceptionMessage(e: Result with ResultStackTrace, level: Int, doIt: Boolean = true)(implicit args: Arguments) = {
    if (doIt) {
      val message = "  "+e.message+" ("+e.location+")"
      val onclick = "toggleImage(this); showHide('"+System.identityHashCode(e)+"')"
      printElem(<div class={"level"+level}><img src="images/collapsed.gif"  onclick={onclick}/>
                 {message}
                </div>)
    } else this
  }

  def printStack(e: ResultStackTrace, level: Int, doIt: Boolean = true)(implicit args: Arguments) = {
    if (doIt) enclose((t: NodeSeq) => <div id={System.identityHashCode(e).toString} style="display:none">{t}</div>) {
      e.stackTrace.foldLeft(blank) { (res, cur) => 
        res.printText(cur.toString, level)
      }
    } else this
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
      {javascript}
      <script language="javascript">window.onload={"init;"}</script>
      <!-- the tabber.js file must be loaded after the onload function has been set, in order to run the
           tabber code, then the init code -->
      <script type="text/javascript" src="./css/tabber.js"></script> 
      <link rel="stylesheet" href="./css/tabber.css" type="text/css" media="screen"/> 
    </head>
  
  def javascript = 
    <script language="javascript">{
    """ 
      function init() {  prettyPrint(); };   
      /* found on : http://www.tek-tips.com/faqs.cfm?fid=6620 */
      String.prototype.endsWith = function(str) { return (this.match(str+'$') == str) };
      function changeWidth(id,width) {  document.getElementById(id).style.width = width; };
      function changeMarginLeft(id, margin) { document.getElementById(id).style.marginLeft = margin; };
      function toggleImage(image) {
        if (image.src.endsWith('images/expanded.gif')) 
          image.src = 'images/collapsed.gif';
        else 
          image.src = 'images/expanded.gif';
      };
      function showHide(id) {
        element = document.getElementById(id);
        element.style.display = (element.style.display == 'none')? 'block' : 'none';
      };
    """
    }</script>


  def flush = {
    val toPrint = 
      if (xml.size == 1) new scala.xml.PrettyPrinter(10000, 2).format(xml(0))
      else if (xml.size > 1) new scala.xml.PrettyPrinter(10000, 2).format(Group(xml))
      else xml.toString
    out.write(toPrint)
  }
}
