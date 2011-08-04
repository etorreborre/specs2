package org.specs2
package reporter
import java.io.Writer
import scala.xml._
import main.Arguments
import text.Markdown._
import text._
import EditDistance._
import NotNullStrings._
import text.Trim._
import execute._
import xml.Nodex._
import matcher.DataTable
import specification._

/**
 * This class stores the html to print to a file (as a NodeSeq object)
 * 
 * An instance of that class is immutable so each print method returns another instance
 * containing more xml to print.
 *
 */
private[specs2]
class HtmlResultOutput(val xml: NodeSeq = NodeSeq.Empty) {
  
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
  private[specs2] lazy val blank = new HtmlResultOutput
  
  def printBr(doIt: Boolean = true)(implicit args: Arguments) = 
    if (doIt) printElem(<status class='ok'><br></br></status>)
    else this
    
  def printPar(text: String = "", doIt: Boolean = true)(implicit args: Arguments) = 
    if (doIt) printElem(<status class='ok'><p>{wiki(text)}</p></status>)
    else this

  def printText(text: String = "", level: Int = 0, doIt: Boolean = true)(implicit args: Arguments) = 
    if (doIt) printElem(<status class='ok'><div class={l(level)}>{wiki(text)}</div></status>)
    else this

  def printTextPar(text: String = "", level: Int = 0, doIt: Boolean = true)(implicit args: Arguments) = 
    if (doIt) printElem(<status class='ok'><p class={l(level)}>{wiki(text)}</p></status>)
    else this

  def printSpecStart(name: SpecName, stats: Stats)(implicit args: Arguments): HtmlResultOutput = {
    printElem(<title>{name.title}</title>).
    printElem(
      if (stats.hasIssues) <h2>{name.title}
        <notoc><a href="#" onclick="hideByClass('ok');hideById('wasIssue');showById('all')"><i id='wasIssue' style="font-size:small">(issues only)</i></a></notoc>
        <notoc><a href="#" onclick="showByClass('ok');hideById('all');showById('wasIssue')"><i id='all' style="display:none;font-size:small">(all)</i></a></notoc></h2>
      else <h2>{name.title}</h2>)
  }

  def l(level: Int)(implicit args: Arguments) = "level" + (if (args.noindent) 0 else level)

  def wiki(text: String)(implicit args: Arguments) = toXhtml(text)

  def printLink(link: HtmlLink, level: Int = 0, stats: Stats)(implicit args: Arguments) = {
    val linkStatus = if (stats.hasIssues) "ko" else "ok"
    link match {
      case slink @ SpecHtmlLink(name, before, link, after, tip) =>
        printElem(<status class={linkStatus}><div class={l(level)}><img src={icon(stats.result.statusName)}/> {wiki(before)}<a href={slink.url} tooltip={tip}>{wiki(link)}</a>{wiki(after)}</div></status>)
      case UrlHtmlLink(url, before, link, after, tip) =>
        printElem(<status class={linkStatus}><div class={l(level)}>{before}<a href={url} tooltip={tip}>{wiki(link)}</a>{wiki(after)}</div></status>)
    }
  }

  def printTextWithIcon(message: MarkupString, iconName: String, level: Int = 0, doIt: Boolean = true)(implicit args: Arguments) =
    if (doIt) printElem(<status class='ok'>{textWithIcon(message, iconName, level)}</status>)
    else this

  def printIssueWithIcon(message: MarkupString, iconName: String, level: Int = 0, doIt: Boolean = true)(implicit args: Arguments) =
    if (doIt) printElem(<status class='ko'>{textWithIcon(message, iconName, level)}</status>)
    else this

  private def textWithIcon(message: MarkupString, iconName: String, level: Int = 0)(implicit args: Arguments) =
    <div class={l(level)}><img src={icon(iconName)}/> {wiki(message.toHtml)}</div>

  def icon(t: String) = "./images/icon_"+t+"_sml.gif"

  def printSuccess(message: MarkupString, level: Int = 0, doIt: Boolean = true)(implicit args: Arguments) =
    printTextWithIcon(message, "success", level, doIt)
  def printFailure(message: MarkupString, level: Int = 0, doIt: Boolean = true)(implicit args: Arguments) =
    printIssueWithIcon(message, "failure", level, doIt)
  def printError  (message: MarkupString, level: Int = 0, doIt: Boolean = true)(implicit args: Arguments) =
    printIssueWithIcon(message, "error",   level, doIt)
  def printSkipped(message: MarkupString, level: Int = 0, doIt: Boolean = true)(implicit args: Arguments) =
    printTextWithIcon(message, "skipped", level, doIt)
  def printPending(message: MarkupString, level: Int = 0, doIt: Boolean = true)(implicit args: Arguments) =
    printTextWithIcon(message, "info",    level, doIt)

  def printExceptionMessage(e: Result with ResultStackTrace, level: Int, doIt: Boolean = true)(implicit args: Arguments) = {
    if (doIt) {
      val message = "  "+e.message+" ("+e.location+")"
      printElem(<status class='ko'><div class={l(level)}>{message}</div></status>)
    } else this
  }
  
  def printCollapsibleExceptionMessage(e: Result with ResultStackTrace, level: Int, doIt: Boolean = true)(implicit args: Arguments) = {
    if (doIt) {
      val message = "  "+e.message.notNull+" ("+e.location+")"
      printElem(<status class='ko'><div class={l(level)}><img src="images/collapsed.gif" onclick={onclick(e)}/>
                 {message}
                </div></status>)
    } else this
  }
  private def onclick(a: Any) = "toggleImage(this); showHide('"+id(a)+"')"
  private def id(a: Any) = System.identityHashCode(a).toString

  def printCollapsibleDetailedFailure(d: Details, level: Int, doIt: Boolean = true)(implicit args: Arguments) = {
    if (doIt) {
      d match {
        case FailureDetails(expected, actual) if args.diffs.show(expected, actual) => {
          val (expectedDiff, actualDiff) = args.diffs.showDiffs(expected, actual)
          val (expectedMessage, actualMessage) = ("Expected: " + expectedDiff, "Actual:   " + actualDiff)
          val (expectedFull, actualFull) = ("Expected (full): " + expected, "Actual (full):   " + actual)
          printElem(<status class='ko'>
<div class={l(level)}><img src="images/collapsed.gif"  onclick={onclick(d)}/>details</div>
  <div id={id(d)} style="display:none">
    <pre class="details">{expectedMessage+"\n"+actualMessage}</pre>
    { if (args.diffs.showFull) <pre class="details">{expectedFull+"\n"+actualFull}</pre> else NodeSeq.Empty }
  </div></status>)
        }
        case _ => this
      }
    } else this
  }

  def printStack(e: ResultStackTrace, level: Int, doIt: Boolean = true)(implicit args: Arguments) = {
    if (doIt) enclose((t: NodeSeq) => <status class='ko'><div id={System.identityHashCode(e).toString} style="display:none">{t}</div></status>) {
      args.traceFilter(e.stackTrace).foldLeft(blank) { (res, cur) =>
        res.printText(cur.toString, level)
      }
    } else this
  }
  def printElem(xml2: Elem, doIt: Boolean = true)(implicit args: Arguments) = {
    if (doIt) new HtmlResultOutput(xml ++ xml2)
    else this
  }
  
  def printNodeSeq(xml2: NodeSeq, doIt: Boolean = true)(implicit args: Arguments) = {
    if (doIt) new HtmlResultOutput(xml ++ xml2)
    else this
  }

  def printHead = new HtmlResultOutput(xml ++ head)
  
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
      <script type="text/javascript" src="css/jquery.js"></script>
      <script type="text/javascript" src="css/jquery.cookie.js"></script>
      <script type="text/javascript" src="css/jquery.hotkeys.js"></script>
      <script type="text/javascript" src="css/jquery.jstree.js"></script>
      <script type="text/javascript" src="./css/tooltip.js"/>
      {javascript}
      <script language="javascript">window.onload={"init;"}</script>
      <!-- the tabber.js file must be loaded after the onload function has been set, in order to run the
           tabber code, then the init code -->
      <script type="text/javascript" src="./css/tabber.js"></script> 
      <link rel="stylesheet" href="./css/tabber.css" type="text/css" media="screen"/> 
    </head>
  
  def javascript = 
    <script language="javascript"><xml:unparsed>
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
        element.style.display = (element.style.display == 'block')? 'none' : 'block';
      };
      function showHideByClass(name) {
		    var elements = document.getElementsByClassName(name);
        for (i = 0; i < elements.length; i++) {
		      elements[i].style.display = (elements[i].style.display == 'none') ? elements[i].style.display = '': 'none';
        }
      };
      function showByClass(name) {
        var elements = document.getElementsByClassName(name);
        for (i = 0; i < elements.length; i++) {
          elements[i].style.display = 'block';
        }
      };
      function hideByClass(name) {
        var elements = document.getElementsByClassName(name);
        for (i = 0; i < elements.length; i++) {
          elements[i].style.display = 'none';
        }
      };
      function showById(id) {
        document.getElementById(id).style.display = ''
      };
      function hideById(id) {
        document.getElementById(id).style.display = 'none'
      };
    </xml:unparsed></script>

  /**
   * writing to the output and doing <br></br> replacement by <br/> to avoid newlines to be inserted after the Xhtml
   * parsing
   */
  def flush(out: Writer) = out.write(Xhtml.toXhtml(xml))
}
