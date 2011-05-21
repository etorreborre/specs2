package org.specs2
package main

/**
 * Utility methods to get systems properties prefixed with specs2
 */
private[specs2]
trait SystemProperties {
  val specs2Prefix = "specs2."
    
  /** @return the value of the system property p */  
  def getProperty(p: String): Option[String] = Option(System.getProperties.get(specs2Prefix + p)).
                               orElse(Option(System.getProperties.get(specs2Prefix + p.toLowerCase))).
                               orElse(Option(System.getProperties.get(p))).
                               orElse(Option(System.getProperties.get(p.toLowerCase))).map(_.toString)

  /** @return the value of the system property p or a default value */
  def getOrElse(p: String, defaultValue: String): String = getProperty(p).getOrElse(defaultValue)
}

private[specs2]
object SystemProperties extends SystemProperties