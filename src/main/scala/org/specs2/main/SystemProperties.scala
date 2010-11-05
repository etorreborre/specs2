package org.specs2
package main

/**
 * Utility methods to get systems properties prefixed with specs2
 */
private[specs2]
trait SystemProperties {
  val specs2Prefix = "specs2."
    
  /** @return the value of the system property p */  
  def getProperty(p: String) = System.getProperties.get(specs2Prefix + p)
  /** @return true if the system property p is defined */
  def isDefined(p: String) = getProperty(p) != null
  /** @return the value of the system property p or a default value */
  def getOrElse(p: String, defaultValue: String): String = 
    Option(getProperty(p)).getOrElse(defaultValue).toString
}

private[specs2]
object SystemProperties extends SystemProperties