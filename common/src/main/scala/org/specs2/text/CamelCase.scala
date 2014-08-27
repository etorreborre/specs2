package org.specs2
package text

import java.lang.StringBuilder

/**
 * This trait does CamelCase transformations on names
 */
private[specs2]
trait CamelCase {

  implicit class CamelCased(s: String) {
    def camelCaseToWords = s.drop(1).foldLeft(new StringBuilder(s.take(1).map(_.toLower))) { (res, cur) =>
      res.append(if (cur.isUpper) " " + cur.toLower else cur)
    }.toString

    def camelCaseToWordsCapitalized = camelCaseToWords.capitalize
  }
}

private[specs2]
object CamelCase extends CamelCase