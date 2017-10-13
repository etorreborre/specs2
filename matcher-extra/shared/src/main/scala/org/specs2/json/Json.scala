package org.specs2.json

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

  /** show JSON objects with null values shown as 'null' */

  def showJson(a: Any): String = a match {
    case map: Map[_, _]  => map.map { case (key, value) => s""""$key":${showJsonValue(value)}"""}.mkString("{", ",", "}")
    case (key, value)    => s"""{"$key":${showJsonValue(value)}}"""
    case JSONObject(map) => showJson(map)
    case JSONArray(list) => list.map(showJsonValue).mkString("[", ",", "]")
    case null            => "null"
    case s: String       => s
    case d: Double       => d.toString
    case b: Boolean      => b.toString
    case other           => other.toString
  }

  /**
   * show JSON values in maps or lists
   * if those values are other JSON objects, recurse with the showJson method
   */
  def showJsonValue(a: Any): String = a match {
    case null            => "null"
    case s: String       => "\""+s+"\""
    case d: Double       => d.toString
    case b: Boolean      => b.toString
    case other           => showJson(other)
  }
  
}
private[specs2]
object Json extends Json
