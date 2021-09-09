package org.specs2.json

/** This trait provides utility functions for Json objects
  */
private[specs2] trait Json:

  /** The string has to be parsed by a brand new parser each time otherwise it will not be thread (see issue #70)
    * @return
    *   Some(json) if the string is parsable as a JSON document
    */
  def parse(s: String): Option[JSONType] =
    def parseRaw(p: Parser, input: String): Option[JSONType] =
      p.phrase(p.root)(new p.lexical.Scanner(input)) match
        case p.Success(result, _) => Some(result)
        case _                    => None
    val parser = new Parser
    // give the parser a chance to parse singly-quoted json
    parseRaw(parser, s).orElse(if s.contains("'") then parseRaw(parser, s.replace("'", "\"")) else None)

  /** show JSON objects with null values shown as 'null' */

  def showJson(a: Any): String = a.asInstanceOf[Matchable] match
    case map: Map[?, ?] =>
      map.map { case (key, value) => s""""$key":${showJsonValue(value)}""" }.mkString("{", ",", "}")
    case (key, value)    => s"""{"$key":${showJsonValue(value)}}"""
    case JSONObject(map) => showJson(map)
    case JSONArray(list) => list.map(showJsonValue).mkString("[", ",", "]")
    case null            => "null"
    case s: String       => s
    case d: Double       => d.toString
    case b: Boolean      => b.toString
    case other           => other.toString

  /** show JSON values in maps or lists if those values are other JSON objects, recurse with the showJson method
    */
  def showJsonValue(a: Any): String = a.asInstanceOf[Matchable] match
    case null       => "null"
    case s: String  => "\"" + s + "\""
    case d: Double  => d.toString
    case b: Boolean => b.toString
    case other      => showJson(other)

private[specs2] object Json extends Json
