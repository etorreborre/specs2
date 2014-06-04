package org.specs2
package text

import java.lang.StringBuilder

/**
 * This trait does CamelCase transformations on names
 */
trait CamelCase {

  implicit def camelCased(s: String) = new CamelCased(s)
  class CamelCased(s: String) {
    def camelCaseToWords = s.drop(1).foldLeft(new StringBuilder(s.take(1).map(_.toLower))) { (res, cur) =>
      res.append(if (cur.isUpper) " " + cur.toLower else cur)
    }.toString
  }
}

object CamelCase extends CamelCase