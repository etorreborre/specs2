package org.specs2
package main
import scala.collection.JavaConversions._
import text.NotNullStrings._
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
                                               orElse(properties.get(p.toLowerCase)).map(_.toString)

  /** @return the value of the system property p or a default value */
  def getOrElse(p: String, defaultValue: String): String = getProperty(p).getOrElse(defaultValue)

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