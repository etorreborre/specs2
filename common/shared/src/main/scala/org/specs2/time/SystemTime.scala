package org.specs2.time

/** This trait provides the current time */
trait SystemTime:
  def nanoTime: Long

object JavaSystemTime extends SystemTime:
  override def nanoTime: Long =
    System.nanoTime()
