package org.specs2
package main
import scala.collection.JavaConversions._
import text.NotNullStrings._
import text.FromString
import java.util.regex.Pattern

/**
 * Utility methods to get systems properties prefixed with specs2
 */
trait SystemProperties {
  val specs2Prefix = "specs2."

  /** copy system properties on first access to avoid possible concurrent modification exceptions later */
  private lazy val systemProperties =
    synchronized(System.getProperties.stringPropertyNames.toList.foldLeft(Map[String, String]()) { (res, key) =>
      res.updated(key,System.getProperty(key))
    })

  /** @return a system property if it exists */
  protected def systemGetProperty(p: String): Option[String] = systemProperties.get(p.notNull)
    
  /** @return the value of the system property p */  
  def getProperty(p: String): Option[String] =        systemGetProperty(specs2Prefix + p).
                                               orElse(systemGetProperty(specs2Prefix + p.toLowerCase)).
                                               orElse(systemGetProperty(p)).
                                               orElse(systemGetProperty(p.toLowerCase)).map(_.notNull)

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
}

object SystemProperties extends SystemProperties

/**
 * This class is used in specifications to mock the system properties
 */
private[specs2]
trait MapSystemProperties extends SystemProperties {
  def properties: Map[String, String]
  override def systemGetProperty(p: String) = properties.get(p)
}