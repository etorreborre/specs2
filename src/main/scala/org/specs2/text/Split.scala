package org.specs2
package text

import Trim._

private[specs2]
trait Split {
  implicit def split(s: String) = new Splitted(s)

  class Splitted(s: String) {

    def splitToSize(n: Int): List[String] = splitToSize(s, n, Nil)
    private def splitToSize(string: String, n: Int, result: List[String]): List[String] = {
      if (string.size <= n) (string :: result).reverse
      else
        // new Strings are necessary to avoid memory errors because substring is just a view on the underlying string
        splitToSize(new String(string.drop(n)), n, new String(string.take(n)) :: result)
    }

    private val quoted = "\"[^\"]*\"|[^\\s]+".r
    def splitQuoted = quoted.findAllIn(s).toSeq.map(_.trimEnclosing("\""))
  }
}
private[specs2]
object Split extends Split
