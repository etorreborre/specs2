package org.specs2
package json

import util.parsing.json._

/**
 * This trait provides utility functions for Json objects
 */
private[specs2]
trait Json {

  /**
   * @return Some(json) if the string is parsable as a JSON document
   */
  def parse(s: String): Option[JSONType] = JSON.parseRaw(s)

  /**
   * @return the list of pairs in the json document where the value is a terminal type
   */
  def pairs(json: JSONType): Seq[(Any, Any)] = json match {
    case JSONObject(map) => map.toList.flatMap { v => v match {
        case (k, o @ JSONObject(map)) => pairs(o)
        case (k, o @ JSONArray(list)) => pairs(o)
        case (key, value)             => Seq((key, value))
      }
    }
    case JSONArray((o @ JSONObject(map)) :: rest) => pairs(o) ++ pairs(JSONArray(rest))
    case JSONArray(other :: rest)                 => pairs(JSONArray(rest))
    case JSONArray(list)                          => Nil
  }

  /**
   * @return the list of values in the json document where the value is a terminal type
   */
  def values(json: JSONType): Seq[Any] = json match {
    case JSONObject(map) => map.toList.flatMap { v => v match {
        case (k, o @ JSONObject(map)) => values(o)
        case (k, o @ JSONArray(list)) => values(o)
        case (key, value)             => Seq(value)
      }
    }
    case JSONArray((o @ JSONObject(map)) :: rest) => values(o) ++ values(JSONArray(rest))
    case JSONArray((o @ JSONArray(list)) :: rest) => values(o) ++ values(JSONArray(rest))
    case JSONArray(other :: rest)                 => Seq(other) ++ values(JSONArray(rest))
    case JSONArray(Nil)                           => Nil
  }

  /**
   * @return the JSON object that's addressed by a key if the document is a Map
   */
  def find(key: String, json: JSONType): Option[JSONType] = json match {
    case JSONObject(map) => map.get(key) match {
      case Some(o @ JSONObject(m)) => Some(o)
      case Some(o @ JSONArray(a))  => Some(o)
      case other                   => None
    }
    case other           => None
  }

  /**
   * @return all the JSON objects or values that are addressed by a key in the document.
   *         if there is only one it is returned directly, otherwise the elements are put in a JSONArray
   */
  def findDeep(key: String, json: JSONType): Option[JSONType] = {
    def findDeepSeq(json: JSONType): Seq[Any] = json match {
      case JSONObject(map) => map.get(key) match {
        case Some(o @ JSONObject(m))   => Seq(o) ++ findDeepSeq(o)
        case Some(o @ JSONArray(list)) => Seq(o) ++ findDeepSeq(o)
        case Some(other)               => Seq(other)
        case None                      => findDeepSeq(JSONArray(map.values.toList))
      }
      case JSONArray((o @ JSONObject(m)) :: rest)   => findDeepSeq(o) ++ findDeepSeq(JSONArray(rest))
      case JSONArray((o @ JSONArray(a)) :: rest)    => findDeepSeq(o) ++ findDeepSeq(JSONArray(rest))
      case JSONArray(other :: rest)                 => findDeepSeq(JSONArray(rest))
      case JSONArray(list)                          => Nil
    }
    val seq = findDeepSeq(json)
    seq match {
      case Nil                        => None
      case (o @ JSONArray(_)) :: Nil  => Some(o)
      case (o @ JSONObject(_)) :: Nil => Some(o)
      case other                      => Some(JSONArray(seq.toList))
    }
  }

}
private[specs2]
object Json extends Json