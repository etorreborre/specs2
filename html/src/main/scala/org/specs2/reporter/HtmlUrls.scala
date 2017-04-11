package org.specs2
package reporter

import io._
import FilePathReader._
import scala.xml.NodeSeq
import java.net.{HttpURLConnection, URL}
import control._
import org.specs2.fp.syntax._
import Exceptions._
import text.Trim._
import html.Htmlx._
import execute.{Success, Failure, Result}
import Result.ResultFailureMonoid

trait HtmlUrls {

  /**
   * check all the urls referenced in <a href="..."/> nodes of a html document having a given filePath.
   *
   * @return a Result, Success or Failure summarizing all checks. In the case of a Failure, only the failure messages are kept
   */
  def check(html: NodeSeq, others: Map[String, NodeSeq] = Map(), rootDirectory: DirectoryPath = DirectoryPath.EMPTY, filePath: FilePath = DirectoryPath.EMPTY.toFilePath): Result =
    urls(html, filePath).foldMap(isAliveResult(_, html, others, rootDirectory)).mapMessage(_.replace("; ", "\n"))

  /** @return true if it is possible to connect to this url through http or locally */
  def isAlive(url: String, html: NodeSeq = NodeSeq.Empty, others: Map[String, NodeSeq] = Map(), rootDirectory: DirectoryPath = DirectoryPath.EMPTY): Boolean =
    isAliveResult(url, html, others, rootDirectory).isSuccess

  /** @return true if the url cannot be accessed through http or on the file system */
  def isDead(url: String, html: NodeSeq = NodeSeq.Empty, others: Map[String, NodeSeq] = Map(), rootDirectory: DirectoryPath = DirectoryPath.EMPTY) =
    !isAlive(url, html, others, rootDirectory)

  /**@return a Success if the url can be accessed, a Failure otherwise */
  def isAliveResult(url: String, html: NodeSeq, others: Map[String, NodeSeq] = Map(), rootDirectory: DirectoryPath = DirectoryPath.EMPTY): Result =
    if (url.startsWith("http")) isAliveHttpResult(url)
    else if (url.startsWith("#")) isAliveAnchorResult(url, html)
    else if (url.contains("#")) isAliveAnchorInFileResult(url, others, rootDirectory)
    else isAliveFileResult(url, others, rootDirectory)


  protected def isAliveHttpResult(url: String) =
    aliveResult(url, isAliveHttp(url))

  protected def isAliveFileResult(url: String, others: Map[String, NodeSeq], rootDirectory: DirectoryPath) =
    aliveResult((rootDirectory / FilePath.unsafe(url)).path, isAliveFile(url, others, rootDirectory))

  protected def isAliveAnchorResult(url: String, html: NodeSeq) =
    aliveResult(url, isAliveAnchor(url, html))

  protected def isAliveAnchorInFileResult(url: String, others: Map[String, NodeSeq], rootDirectory: DirectoryPath) =
    aliveResult(url, isAliveAnchorInFile(url, others, rootDirectory))

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
  def isAliveFile(url: String, others: Map[String, NodeSeq], rootDirectory: DirectoryPath) =
    others.keys.exists(o => o == url) || exists(rootDirectory / FilePath.unsafe(url)).runOption.getOrElse(false)

  /**@return true if the url is an anchor in the document */
  def isAliveAnchor(url: String, html: NodeSeq) =
    (html \\ "a").map(a => a.attribute("name").mkString).contains(url.removeFirst("#"))

  /**
   * This method is used when the html file can't be parsed
   * @return true if the url is an anchor in a String document
   */
  def isAliveAnchor(url: String, html: String) = html.contains("name=\"" + url.removeFirst("#") + "\"")

  /**
   * look for the anchor in another file to be written to disk by specs2 or a static file already generated
   * @return true if the url is found
   */
  def isAliveAnchorInFile(url: String, others: Map[String, NodeSeq], rootDirectory: DirectoryPath) = {
    val (file, anchor) = (url.split("#")(0), url.split("#")(1))
    isAliveFile(file, others, rootDirectory) &&
      (isAliveAnchor(anchor, others.find { case (k, v) => k == file }.fold(NodeSeq.Empty)(_._2)) ||
        isAliveAnchor(anchor, readFile(rootDirectory / FilePath.unsafe(file)).runOption.getOrElse("")))
  }
}

object HtmlUrls extends HtmlUrls
