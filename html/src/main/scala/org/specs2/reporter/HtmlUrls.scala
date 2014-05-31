package org.specs2
package reporter

import io.FileSystem
import scala.xml.NodeSeq
import java.net.{HttpURLConnection, URL}
import scalaz.Scalaz
import Scalaz._
import collection.Seqx._
import control.Exceptions._
import io.Paths._
import text.Trim._
import html.Htmlx._
import execute.{Success, Failure, Result}
import Result.ResultFailureMonoid

trait HtmlUrls extends FileSystem {

  /**
   * check all the urls referenced in <a href="..."/> nodes of a html document having a given filePath.
   *
   * @return a Result, Success or Failure summarizing all checks. In the case of a Failure, only the failure messages are kept
   */
  def check(html: NodeSeq, others: Map[String, NodeSeq] = Map(), rootPath: String = ".", filePath: String = ""): Result =
    urls(html, filePath).foldMap(isAliveResult(_, html, others, rootPath)).mapMessage(_.replace("; ", "\n"))

  /**@return true if it is possible to connect to this url through http or locally */
  def isAlive(url: String, html: NodeSeq = NodeSeq.Empty, others: Map[String, NodeSeq] = Map(), rootPath: String = "."): Boolean =
    isAliveResult(url, html, others, rootPath).isSuccess

  /**@return true if the url cannot be accessed through http or on the file system */
  def isDead(url: String, html: NodeSeq = NodeSeq.Empty, others: Map[String, NodeSeq] = Map(), root: String = ".") =
    !isAlive(url, html, others, root)

  /**@return a Success if the url can be accessed, a Failure otherwise */
  def isAliveResult(url: String, html: NodeSeq, others: Map[String, NodeSeq] = Map(), rootPath: String = "."): Result =
    if (url.startsWith("http")) isAliveHttpResult(url)
    else if (url.startsWith("#")) isAliveAnchorResult(url, html)
    else if (url.contains("#")) isAliveAnchorInFileResult(url, others, rootPath)
    else isAliveFileResult(url, others, rootPath)


  protected def isAliveHttpResult(url: String) = aliveResult(url, isAliveHttp(url))

  protected def isAliveFileResult(url: String,
                                  others: Map[String, NodeSeq],
                                  rootPath: String) = aliveResult(rootPath.dirPath + url, isAliveFile(url, others, rootPath))

  protected def isAliveAnchorResult(url: String, html: NodeSeq) = aliveResult(url, isAliveAnchor(url, html))

  protected def isAliveAnchorInFileResult(url: String,
                                          others: Map[String, NodeSeq],
                                          rootPath: String) = aliveResult(url, isAliveAnchorInFile(url, others, rootPath))

  protected def aliveResult(url: String, condition: Boolean): Result =
    if (condition) Success(url + " is alive")
    else           Failure(url + " is dead")

  /**
   * @return true if the url can be accessed through http
   */
  protected def isAliveHttp(url: String) = {
    tryo {
      val huc = new URL(url).openConnection.asInstanceOf[HttpURLConnection]
      huc.connect()
      Seq(HttpURLConnection.HTTP_OK, HttpURLConnection.HTTP_MOVED_TEMP).contains(huc.getResponseCode)
    } getOrElse false
  }

  /**@return true if the url can be accessed on the file system */
  def isAliveFile(url: String, others: Map[String, NodeSeq], root: String) =
    others.keys.exists(o => o.samePathAs(url)) || exists(root.dirPath + url)

  /**@return true if the url is an anchor in the document */
  def isAliveAnchor(url: String, html: NodeSeq) = (html \\ "a").map(a => a.attribute("name").mkString).contains(url.removeFirst("#"))

  /**
   * This method is used when the html file can't be parsed
   * @return true if the url is an anchor in a String document
   */
  def isAliveAnchor(url: String, html: String) = html.contains("name=\"" + url.removeFirst("#") + "\"")

  /**
   * look for the anchor in another file to be written to disk by specs2 or a static file already generated
   * @return true if the url is found
   */
  def isAliveAnchorInFile(url: String, others: Map[String, NodeSeq], root: String) = ???
//  {
//    val (file, anchor) = (url.split("#")(0), url.split("#")(1))
//    isAliveFile(file, others, root) &&
//      (isAliveAnchor(anchor, others.find { case (k, v) => k.samePathAs(file)}.fold(NodeSeq.Empty)(_._2)) ||
//        isAliveAnchor(anchor, loadXhtmlFile(root.dirPath+file, silentLoadXhtmlFileReport, sourceErrors = false)) ||
//        isAliveAnchor(anchor, readFile(root.dirPath+file)))
//  }
}

object HtmlUrls extends HtmlUrls