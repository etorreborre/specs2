package org.specs2
package json

/**
 * This trait provides utility functions for Json objects
 */
private[specs2]
trait Json {

  /**
   * The string has to be parsed by a brand new parser each time
   * otherwise it will not be thread (see issue #70)
   * @return Some(json) if the string is parsable as a JSON document
   */
  def parse(s: String): Option[JSONType] = {
    val parser = new Parser {
      def parseRaw(input : String) : Option[JSONType] =
        phrase(root)(new lexical.Scanner(input)) match {
          case Success(result, _) => Some(result)
          case _ => None
      }
    }
    // give the parser a chance to parse singly-quoted json
    parser.parseRaw(s).orElse(if (s.contains("'")) parser.parseRaw(s.replace("'", "\"")) else None)
  }

}
private[specs2]
object Json extends Json