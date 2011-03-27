package org.specs2
package main

/**
 * Utility methods to get systems properties prefixed with specs2
 */
private[specs2]
trait SystemProperties {
  val specs2Prefix = "specs2."
    
  /** @return the value of the system property p */  
  def getProperty(p: String) = Option(System.getProperties.get(specs2Prefix + p)).orElse(Option(System.getProperties.get(p)))
  /** @return the value of the system property p or a default value */
  def getOrElse(p: String, defaultValue: String): String = getProperty(p).getOrElse(defaultValue).toString
}

private[specs2]
object SystemProperties extends SystemProperties