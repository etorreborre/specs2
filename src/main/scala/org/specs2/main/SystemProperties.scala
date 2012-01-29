package org.specs2
package main
import scala.collection.JavaConversions._
import text.NotNullStrings._
import text.FromString

/**
 * Utility methods to get systems properties prefixed with specs2
 */
private[specs2]
trait SystemProperties {
  val specs2Prefix = "specs2."

  lazy val properties: Map[String, String] = Map(System.getProperties.toSeq.map(s => s._1.notNull -> s._2.notNull):_*)

  /** @return the value of the system property p */
  def getProperty(p: String): Option[String] =        properties.get(specs2Prefix + p).
                                               orElse(properties.get(specs2Prefix + p.toLowerCase)).
                                               orElse(properties.get(p)).
                                               orElse(properties.get(p.toLowerCase)).map(_.notNull)

  /** @return the value of the system property p as a given type */
  def getPropertyAs[T: FromString](p: String): Option[T] = getProperty(p) flatMap implicitly[FromString[T]].fromString

  /** @return the value of the system property p or a default value */
  def getOrElse(p: String, defaultValue: String): String = getProperty(p).getOrElse(defaultValue)

  /** @return the value Some(T) if the property is defined */
  def getIf[T](p: String, value: =>T): Option[T] = getProperty(p).map(v => value)

  /** @return the value Some(T) if the property is defined */
  def getIfElse[T](p: String, v1: =>T)(v2: =>T): T = getIf(p, v1).getOrElse(v2)

  /** @return true if a property is defined */
  def isDefined(p: String) = getProperty(p).isDefined
  /** @return true if there is a property matching the regular expression */
  def areDefined(pattern: String) = properties.keys.exists { p =>
    (p matches pattern) ||
    (p matches (specs2Prefix + pattern))
  }
}

private[specs2]
object SystemProperties extends SystemProperties

/**
 * This class is used in specifications to mock the system properties
 */
private[specs2]
case class MapSystemProperties(map: (String, String)*) extends SystemProperties {
  override lazy val properties = Map(map:_*)
}