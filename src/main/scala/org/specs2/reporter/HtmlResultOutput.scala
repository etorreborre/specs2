package org.specs2
package reporter

import scala.xml._
import control._
import main.{ Arguments, Diffs }
import text.Markdown._
import text._
import io.Paths._
import NotNullStrings._
import text.Trim._
import execute._
import xml.Nodex._
import specification._
import html.Htmlx._

/**
 * This class stores the html to print to a file (as a NodeSeq object)
 * 
 * An instance of that class is immutable so each print method returns another instance
 * containing more xml to print.
 *
 */
private[specs2]
case class HtmlResultOutput(xml: NodeSeq = NodeSeq.Empty, filePath: String = "", customTextPrinter: Option[(String, MarkdownOptions) => NodeSeq] = None)(implicit args: Arguments) extends HtmlReportOutput { outer =>

  protected lazy val textPrinter = customTextPrinter getOrElse ((s: String, options: MarkdownOptions) => toXhtml(s, options)(args))
  
  /**
   * start of the output
   */
  private[specs2] lazy val blank = new HtmlResultOutput(NodeSeq.Empty, outer.filePath, None)
  /** set the file path of the file being written */
  def filePathIs(path: String) = copy(filePath = path)
  /** base directory for this file path */
  def baseDir = filePath.baseDir
  /** print the NodeSeq inside the html tags */
  def printHtml(n: =>NodeSeq) = print(<html>{n}</html>)
  /** print the NodeSeq inside the body tags, with anchors for header tags */
  def printBody(n: =>NodeSeq) = print((<body>{n}</body>).addHeadersAnchors)
  /** print the head of the document */
  def printHead(title: String) = print(xml ++ head(title))

  /** print a br tag */
  def printBr                                         = printOkStatus(<br></br>)
  /** print a paragraph for some text */
  def printPar(text: String = "")                     = printOkStatus(<p>{wiki(text)}</p>)
  /** print some text */
  def printText(text: String = "", level: Int = 0)    = printOkStatus(div(wiki(text), level))
  /** print some text */
  def printText(text: FormattedString, level: Int)    = printOkStatus(div(wiki(text), level))
  /** print some text in a paragraph */
  def printTextPar(text: String = "", level: Int = 0) = printOkStatus(p(wiki(text), level))

  /**
   * print the xhtml for a specification start:
   *
   * - the html title is the specification title
   * - if there are no issues then print a h2 header with the title of the specification
   * - if there are issues add a toggle link so that only issues can be displayed
   */
  def printSpecStart(name: SpecName, stats: Stats) = {
    print(<title>{name.title}</title>).
    print {
     val header =
       if (stats.hasIssues)
        <h2>{name.title}
          <notoc>{showOnlyShowAllLinks(elementClass = "ok", "(issues only)", "(all)")}</notoc>
        </h2>
      else
        <h2>{name.title}</h2>

      header.updateHeadAttribute("specId", name.id)
    }
  }

  /**
   * print a link
   *
   * - if it is a link to another specification:
   *   - if it has issues, set a corresponding icon
   *   - add a subtoc element with the specification id so that the TableOfContents class knows where to insert the sub table content corresponding
   *     to the linked specification when building the global table of contents
   *
   * - if this is an arbitrary link, print a normal html link
   */
  def printLink(link: HtmlLink, level: Int, stats: Stats = Stats(), hidden: Boolean = false) = {
    val linkStatus = if (stats.hasIssues) "ko" else "ok"
    val htmlLink = if (hidden) NodeSeq.Empty else outer.copy(xml = NodeSeq.Empty).printLink(link).xml
    link match {
      case slink @ SpecHtmlLink(name, before, l, after, tip) => {
        val subtoc = print(<subtoc specId={name.id.toString}/>)
        if (hidden) subtoc
        else        subtoc.printStatus(div(<img src={icon(stats.result.statusName)}/> ++ t(" ") ++ htmlLink, level), linkStatus)
      }
      case UrlHtmlLink(url, before, l, after, tip) if !hidden => printStatus(div(htmlLink, level), linkStatus)
      case _                                                  => this
    }
  }

  def printLink(link: HtmlLink) =
    print(wiki(if (link.beforeText.isEmpty) "" else (link.beforeText+" ")) ++
          <a href={link.url.relativeTo(filePath)} tooltip={link.tip}>{wiki(link.linkText)}</a> ++
          wiki(if (link.afterText.isEmpty) "" else (" " +link.afterText+" ")))

  /** print some text with a status icon (with an ok class) */
  def printTextWithIcon(message: FormattedString, iconName: String, level: Int = 0)  = printOkStatus(textWithIcon(message, iconName, level))
  /** print some xml with a status icon (with an ok class) */
  def printOkXmlWithIcon(xml: NodeSeq, iconName: String, level: Int = 0)  = printOkStatus(xmlWithIcon(xml, iconName, level))
  /** print some xml with a status icon (with an ok class) */
  def printKoXmlWithIcon(xml: NodeSeq, iconName: String, level: Int = 0)  = printKoStatus(xmlWithIcon(xml, iconName, level))
  /** print an issue with a status icon (with a ko class) */
  def printIssueWithIcon(message: FormattedString, iconName: String, level: Int = 0) = printKoStatus(textWithIcon(message, iconName, level))
  /** print an exception message (with a ko class) */
  def printExceptionMessage(e: Result with ResultStackTrace, level: Int)          = printKoStatus(div("  "+e.message+" ("+e.location+")", level))

  /**
   * print a collapsible exception message (with a ko class)
   *
   * the message in enclosed in a div which has a unique id and associated onclick function to show/hide it
   */
  def printCollapsibleExceptionMessage(e: Result with ResultStackTrace, level: Int) =
    printKoStatus(div(<img src={baseDir+"images/collapsed.gif"} onclick={toggleElement(e)}/> ++
		                   t("  "+e.message.notNull+" ("+e.location+")"), level))

  /**
   * print the details of a Failure message in a collapsible div
   */
  def printDetailedFailure(details: Details, level: Int, diffs: Diffs) = {
    details match {
      case FailureDetails(expected, actual) if diffs.show(expected, actual) => {
        val (expectedDiff, actualDiff) = diffs.showDiffs(expected, actual)
        val (expectedMessage, actualMessage) = ("Expected: " + expectedDiff, "Actual:   " + actualDiff)
        val (expectedFull, actualFull) = ("Expected (full): " + expected, "Actual (full):   " + actual)
        
				printKoStatus(div(<img src={baseDir+"images/collapsed.gif"}  onclick={toggleElement(details)}/> ++ t("details"), level) ++
          <div id={id(details)} style="display:none">
            <pre class="details">{expectedMessage+"\n"+actualMessage}</pre>
            { <pre class="details">{expectedFull+"\n"+actualFull}</pre> unless (diffs.showFull)  }
          </div>)
      }
      case _ => this
    }
  }

  /**
   * print a stacktrace for a failure or an exception
   *
   * The stacktrace elements are filtered with the traceFilter parameter
   */
  def printStack(e: ResultStackTrace, level: Int, traceFilter: StackTraceFilter) =
    enclose((t: NodeSeq) => koStatus(<div id={System.identityHashCode(e).toString} style="display:none">{t}</div>)) {
      traceFilter(e.stackTrace).foldLeft(blank) { (res, cur) => res.printText(cur.toString, level) }
    }

  /**
   * print the final statistics for a specification as a table
   */
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

  /**
   * print the html for a Form, by just adding the corresponding xml to the current output
   */
	def printOkForm(form: NodeSeq) = print(okStatus(form))
  def printKoForm(form: NodeSeq) = print(koStatus(form))

  protected def printOkStatus(n: NodeSeq) = print(okStatus(n))
	protected def printKoStatus(n: NodeSeq) = print(koStatus(n))
	protected def printStatus(n: NodeSeq, st: String) = print(status(n, st))

  protected def textWithIcon(message: FormattedString, iconName: String, level: Int = 0) = div(<img src={icon(iconName)}/> ++ t(" ") ++ wiki(message) ++ <br/>, level)
  protected def xmlWithIcon(xml: NodeSeq, iconName: String, level: Int = 0) = div(<table class="exampleTable"><td><img src={icon(iconName)}/></td><td>{xml}</td></table>, level)
  protected def icon(t: String) = baseDir+"images/icon_"+t+"_sml.gif"

	protected def okStatus(n: NodeSeq) = status(n, "ok")
	protected def koStatus(n: NodeSeq) = status(n, "ko")
  /** print a NodeSeq with a given status class */
	protected def status(n: NodeSeq, st: String) = <status class={st}>{n}</status>
  /** create a div around some markup text to be displayed at a certain level of indentation */
	protected def div(string: String, level: Int): NodeSeq  = div(t(string), level)
  /** create a div around a NodeSeq to be displayed at a certain level of indentation */
	protected def div(n: NodeSeq, level: Int, hidden: Boolean = false): NodeSeq =
    <div  style={"display: "+(if (hidden) "none" else "show")+s"; text-indent:${level*5}px;"}>{n}</div>

  /** create a paragraph around a NodeSeq to be displayed at a certain level of indentation */
	protected def p(n: NodeSeq, level: Int) = <p class={"level"+level}>{n}</p>
  /** create a Text node */
	protected def t(text: String): NodeSeq = scala.xml.Text(text)
  protected def toggleElement(a: Any) = "toggleImage(this); showHide('"+id(a)+"')"
  protected def id(a: Any) = System.identityHashCode(a).toString
  /** render some markup text as xhtml */
  protected def wiki(text: String) = textPrinter(text, MarkdownOptions())
  protected def wiki(text: FormattedString) =
    if (text.formatting.markdown) textPrinter(text.raw, MarkdownOptions(verbatim = text.formatting.verbatim)) else text.toXml


  /**
   * Head of the html document. It contains:
   *
   *  - links to the prettify css and javascript functions to render code
   *  - jquery scripts to render the table of contents as a tree
   *  - tabber css and scripts to display tabs
   *  - show and hide functions
   */
  def head(title: String) =
    <head>
      <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
      <style type="text/css" media="all">
        {"@import url('"+baseDir+"css/maven-base.css');"}
        {"@import url('"+baseDir+"css/maven-theme.css');"}
      </style>
      <link href={baseDir+"css/prettify.css"} type="text/css" rel="stylesheet" />
      <script type="text/javascript" src={baseDir+"css/prettify.js"}></script>
      <link rel="stylesheet" href={baseDir+"css/print.css"} type="text/css" media="print" />
      <link href={baseDir+"css/tooltip.css"} rel="stylesheet" type="text/css" />
      <link href={baseDir+"css/specs2-user.css"} type="text/css" rel="stylesheet" />

      <script type="text/javascript" src={baseDir+"css/jquery.js"}></script>
      <script type="text/javascript" src={baseDir+"css/jquery.cookie.js"}></script>
      <script type="text/javascript" src={baseDir+"css/jquery.hotkeys.js"}></script>
      <script type="text/javascript" src={baseDir+"css/jquery.jstree.js"}></script>
      <script type="text/javascript" src={baseDir+"css/tooltip.js"}/>
      {javascript}
      <script language="javascript">window.onload={"init;"}</script>
      <!-- the tabber.js file must be loaded after the onload function has been set, in order to run the
           tabber code, then the init code -->
      <script type="text/javascript" src={baseDir+"css/tabber.js"}></script>
      <link rel="stylesheet" href={baseDir+"css/tabber.css"} type="text/css" media="screen"/>
      <title>{title}</title>
    </head>

  /**
   * define custom javascript functions to manipulate elements on the page, mostly to show and hide elements
   */
  def javascript =
    <script language="javascript"><xml:unparsed>
      function init() {  prettyPrint(); };
      /* found on : http://www.tek-tips.com/faqs.cfm?fid=6620 */
      String.prototype.endsWith = function(str) { return (this.match(str+'$') == str) };
      function changeWidth(id,width) {  document.getElementById(id).style.width = width; };
      function changeMarginLeft(id, margin) { document.getElementById(id).style.marginLeft = margin; };
      function toggleImage(image) {
        if (image.src.endsWith('images/expanded.gif')) 
          image.src = image.src.replace('expanded', 'collapsed');
        else 
          image.src = image.src.replace('collapsed', 'expanded');
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
  def print(xml2: NodeSeq): HtmlResultOutput = copy(xml = xml ++ xml2)
  def print(xml2: Elem): HtmlResultOutput = copy(xml = xml ++ xml2)

  /**
   * @param elementClass class of elements to show/hide
   * @param showOnlyLabel label to display for the elements to show only
   * @param showAllLabel label to display for showing all the elements
   *
   * @return 2 links allowing to show/hide some elements on a html page
   */
  protected def showOnlyShowAllLinks(elementClass: String, showOnlyLabel: String, showAllLabel: String): NodeSeq = {
    val (showOnlyId, showAllId) = ("onlyIssuesLink", "allElementsLink")
    val showOnlyOnclick = "hideByClass('"+elementClass+"');hideById('"+showOnlyId+"');showById('"+showAllId+"')"
    val showAllOnclick  = "showByClass('"+elementClass+"');hideById('"+showAllId+"');showById('"+showOnlyId+"')"
    <a href="#" onclick={showOnlyOnclick}><i id={showOnlyId} style="font-size:small">{showOnlyLabel}</i></a> ++
    <a href="#" onclick={showAllOnclick}><i id={showAllId} style="display:none;font-size:small">{showAllLabel}</i></a>
  }


}

