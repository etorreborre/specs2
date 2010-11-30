package org.specs2
package reporter
import scala.xml._
import java.io.Writer
import main.Arguments

class HtmlResultOutput(out: Writer, val xml: NodeSeq = NodeSeq.Empty) {
  def printPar(text: String = "", doIt: Boolean = true)(implicit args: Arguments) = 
    if (doIt) printElem(<p>{text}</p>)
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
  def flush = out.write(xml.toString)
}
