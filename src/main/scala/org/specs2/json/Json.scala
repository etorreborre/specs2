package org.specs2
package json

import util.parsing.json._
import util.matching.Regex
import text.Regexes._
import text.NotNullStrings._

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
  def parse(s: String): Option[JSONType] =
    new Parser {
    def parseRaw(input : String) : Option[JSONType] =
      phrase(root)(new lexical.Scanner(input)) match {
        case Success(result, _) => Some(result)
        case _ => None
      }
  }.parseRaw(s)

  /**
   * @return the list of pairs in the json document where the value is a terminal type
   */
  def terminalPairs(json: JSONType): Seq[(Any, Any)] = collect(json)(keyedValues = {
    case (key, value) => Seq((key,value))
    case other        => Nil
  })

  /**
   * @return the list of values in the json document where the value is a terminal type
   */
  def terminalValues(json: JSONType): Seq[Any] = collect(json)(keyedValues = {
    case (key, value) => Seq(value)
    case other        => Seq(other)
  })

  /**
   * @return all the JSON objects or values that are addressed by a key in the document.
   *         if there is only one it is returned directly, otherwise the elements are put in a JSONArray
   */
  def findDeep(key: String, json: JSONType): Option[JSONType] = findDeep(json, (k: Any) => k == key)
  /**
   * @return findDeep with a Regex
   */
  def findDeep(key: Regex, json: JSONType): Option[JSONType] = findDeep(json, (k: Any) => key matches k.toString)
  /**
   * @return findDeep with a Regex or a String
   */
  def findDeep(key: Any, json: JSONType): Option[JSONType] = key match {
    case k: Regex => findDeep(k, json)
    case other    => findDeep(other.notNull, json)
  }

  /**
   * @return findDeep with a key equality function
   */
  def findDeep(json: JSONType, targetKey: Any => Boolean): Option[JSONType] = {
    val seq = collect(json)( keyedValues = {
      case (k, v) if targetKey(k) => Seq(v)
      case other                => Nil
    }, keyedObjects = {
      case (k, o) if targetKey(k) => Seq(o)
      case other                => Nil
    })
    seq match {
      case Nil                   => None
      case (o: JSONType) :: Nil  => Some(o)
      case other                 => Some(JSONArray(seq.toList))
    }
  }

  /**
   * @return the JSON object that's addressed by a key if the document is a Map
   */
  def find(key: String, json: JSONType): Option[JSONType] = json match {
    case JSONObject(map) => map.get(key) map { case (o: JSONType) => o }
    case other           => None
  }

  /**
   * @return the JSON object that's addressed by a key if the document is a Map, and the key matches a regular expression
   */
  def find(keyRegex: Regex, json: JSONType): Option[JSONType] = json match {
    case JSONObject(map) => map.iterator.toSeq.collect { case (k, v) if keyRegex matches k => v }.headOption map { case (o: JSONType) => o }
    case other           => None
  }

  /**
   * @return find with a Regex or a String
   */
  def find(key: Any, json: JSONType): Option[JSONType] = key match {
    case k: Regex => find(k, json)
    case other    => find(other.notNull, json)
  }

  /**
   * generic collect operation to iterate through the JSON tree and get values and objects
   */
  private def collect[T](json: JSONType)(values: Any => Seq[T] = vs,
                                         objects: JSONType => Seq[T] = os,
                                         keyedValues: (Any, Any)  => Seq[T] = kvs,
                                         keyedObjects: (Any, Any) => Seq[T] = kvs): Seq[T] = json match {
    case JSONObject(map) => map.toList.flatMap { v => v match {
        case (k, (o: JSONType)) => objects(o) ++ keyedObjects(k, o) ++ collect(o)(values, objects, keyedValues, keyedObjects)
        case (k, v)             => values(v)  ++ keyedValues(k, v)
      }
    }
    case JSONArray((o: JSONType) :: rest) => objects(o) ++
                                             collect(o)(values, objects, keyedValues, keyedObjects) ++
                                             collect(JSONArray(rest))(values, objects, keyedValues, keyedObjects)
    case JSONArray(other :: rest)         => values(other) ++
                                             collect(JSONArray(rest))(values, objects, keyedValues, keyedObjects)
    case JSONArray(Nil)                   => Nil
  }

  private def vs[T]  = (a: Any) => (Nil:Seq[T])
  private def os[T]  = (a: JSONType) => (Nil:Seq[T])
  private def kvs[T] = (k: Any, v: Any) => (Nil:Seq[T])
}
private[specs2]
object Json extends Json