package org.specs2
package html
import HtmlUrls._
import scala.xml.NodeSeq
import java.net.{URL, HttpURLConnection}
import mutable.Specification
import matcher.ResultMatchers

class HtmlUrlsSpec extends Specification with ResultMatchers {
  skipAllIf(isDead("http://www.google.com"))
  "it is possible to check the links of an html document" >> {
    check(<html><a href="http://www.google.com"></a></html>) must beSuccessful
  }
}

import internal.scalaz._
import Scalaz._
import Foldable._
import execute.{Result, Success, Failure}
import Htmlx._
import Result.ResultFailureMonoid
import control.Identityx._

object HtmlUrls extends HtmlUrls

trait HtmlUrls {

  def check(html: NodeSeq): Result = urls(html).foldMap(isAliveResult(_))

  def isAliveResult(url: String): Result =
    if (isAlive(url)) Success(url + " is alive")
    else              Failure(url + " is dead")

  /**
   * @return true if it is possible to connect to this url
   */
  def isAlive(url: String): Boolean = {
    val huc = new URL(url).openConnection.asInstanceOf[HttpURLConnection]
    huc.setRequestMethod("HEAD")
    huc.connect()
    huc.getResponseCode == HttpURLConnection.HTTP_OK
  }

  def isDead(url: String) = !isAlive(url)
}