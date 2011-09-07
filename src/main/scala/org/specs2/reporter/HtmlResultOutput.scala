package org.specs2
package reporter
import java.io.Writer
import scala.xml._
import control._
import Exceptions._
import main.{ Arguments, Diffs }
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
case class HtmlResultOutput(xml: NodeSeq = NodeSeq.Empty) extends HtmlReportOutput {

  private[specs2] lazy val blank = new HtmlResultOutput
  
  def printHtml(n: =>NodeSeq) = print(<html>{n}</html>)
  def printBody(n: =>NodeSeq) = print(<body>{n}</body>)
  def printHead = print(xml ++ head)
	
  def printBr                                         = printOkStatus(<br></br>)
  def printPar(text: String = "")                     = printOkStatus(<p>{wiki(text)}</p>)
  def printText(text: String = "", level: Int = 0)    = printOkStatus(div(wiki(text), level))
  def printTextPar(text: String = "", level: Int = 0) = printOkStatus(p(wiki(text), level))

  def printSpecStart(name: SpecName, stats: Stats) = {
    print(<title>{name.title}</title>).
    print(
      if (stats.hasIssues) <h2>{name.title}
        <notoc><a href="#" onclick="hideByClass('ok');hideById('wasIssue');showById('all')"><i id='wasIssue' style="font-size:small">(issues only)</i></a></notoc>
        <notoc><a href="#" onclick="showByClass('ok');hideById('all');showById('wasIssue')"><i id='all' style="display:none;font-size:small">(all)</i></a></notoc></h2>
      else <h2>{name.title}</h2>)
  }

  def printLink(link: HtmlLink, level: Int = 0, stats: Stats) = {
    val linkStatus = if (stats.hasIssues) "ko" else "ok"
    link match {
      case slink @ SpecHtmlLink(name, before, link, after, tip) =>
        printStatus(div(<img src={icon(stats.result.statusName)}/> ++ t(" ") ++ wiki(before) ++ <a href={slink.url} tooltip={tip}>{wiki(link)}</a> ++ wiki(after), level), linkStatus)
      case UrlHtmlLink(url, before, link, after, tip) =>
        printStatus(div(t(before) ++ <a href={url} tooltip={tip}>{wiki(link)}</a> ++ wiki(after), level), linkStatus)
    }
  }

  def printTextWithIcon(message: MarkupString, iconName: String, level: Int = 0)  = printOkStatus(textWithIcon(message, iconName, level))
  def printIssueWithIcon(message: MarkupString, iconName: String, level: Int = 0) = printKoStatus(textWithIcon(message, iconName, level))
  def printExceptionMessage(e: Result with ResultStackTrace, level: Int)          = printKoStatus(div("  "+e.message+" ("+e.location+")", level))
  
  def printCollapsibleExceptionMessage(e: Result with ResultStackTrace, level: Int) =
    printKoStatus(div(<img src="images/collapsed.gif" onclick={onclick(e)}/> ++ 
		                   t("  "+e.message.notNull+" ("+e.location+")"), level))

  def printDetailedFailure(details: Details, level: Int, diffs: Diffs) = {
    details match {
      case FailureDetails(expected, actual) if diffs.show(expected, actual) => {
        val (expectedDiff, actualDiff) = diffs.showDiffs(expected, actual)
        val (expectedMessage, actualMessage) = ("Expected: " + expectedDiff, "Actual:   " + actualDiff)
        val (expectedFull, actualFull) = ("Expected (full): " + expected, "Actual (full):   " + actual)
        
				printKoStatus(div(<img src="images/collapsed.gif"  onclick={onclick(details)}/> ++ t("details"), level) ++
          <div id={id(details)} style="display:none">
            <pre class="details">{expectedMessage+"\n"+actualMessage}</pre>
            { <pre class="details">{expectedFull+"\n"+actualFull}</pre> unless (diffs.showFull)  }
          </div>)
      }
      case _ => this
    }
  }

  def printStack(e: ResultStackTrace, level: Int, traceFilter: StackTraceFilter) =
    enclose((t: NodeSeq) => koStatus(<div id={System.identityHashCode(e).toString} style="display:none">{t}</div>)) {
      traceFilter(e.stackTrace).foldLeft(blank) { (res, cur) =>
        res.printText(cur.toString, level)
      }
    }

  def printStats(n: SpecName, stats: Stats) = {

    val title = "Total for specification" + ((" "+n.name.trim) unless n.name.isEmpty)

    val classStatus = if (stats.hasIssues) "failure" else "success"
    print(
      <table class="dataTable">
        <tr><th colSpan="2">{title}</th></tr>
        <tr><td>Finished in</td><td class="info">{stats.time}</td></tr>
        <tr><td>Results</td><td class={classStatus}>{ stats.displayResults(Arguments("nocolor")) }</td></tr>
      </table>)
  }

	def printForm(form: NodeSeq) = print(form)
	
	protected def printOkStatus(n: NodeSeq) = print(okStatus(n))
	protected def printKoStatus(n: NodeSeq) = print(koStatus(n))
	protected def printStatus(n: NodeSeq, st: String) = print(status(n, st))

  protected def textWithIcon(message: MarkupString, iconName: String, level: Int = 0) = div(<img src={icon(iconName)}/> ++ t(" ") ++ wiki(message.toHtml), level)
  protected def icon(t: String) = "./images/icon_"+t+"_sml.gif"

	protected def okStatus(n: NodeSeq) = status(n, "ok")
	protected def koStatus(n: NodeSeq) = status(n, "ko")
	protected def status(n: NodeSeq, st: String) = <status class={st}>{n}</status>
	protected def div(string: String, level: Int): NodeSeq  = div(t(string), level)
	protected def div(n: NodeSeq, level: Int): NodeSeq = <div class={"level"+level}>{n}</div>
	protected def p(n: NodeSeq, level: Int) = <p class={"level"+level}>{n}</p>
	protected def t(text: String): NodeSeq = scala.xml.Text(text)
  protected def onclick(a: Any) = "toggleImage(this); showHide('"+id(a)+"')"
  protected def id(a: Any) = System.identityHashCode(a).toString
  protected def wiki(text: String) = toXhtml(text)

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
   * Usage: out.enclose((t: NodeSeq) => <body>{t}</body>)(<div>inside</div>))
   *
   * to create <body><div>inside</div></body>)
   *
   * @return some xml (rest) enclosed in another block
   */
  private def enclose(f: NodeSeq => NodeSeq)(rest: =>HtmlResultOutput): HtmlResultOutput = print(f(rest.xml))
  private def print(xml2: NodeSeq): HtmlResultOutput = HtmlResultOutput(xml ++ xml2)
  private def print(xml2: Elem): HtmlResultOutput = HtmlResultOutput(xml ++ xml2)

}

