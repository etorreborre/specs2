package org.specs2.data

import org.specs2.fp.*
import org.specs2.control.*
import org.specs2.time.*

/** LRU (least recently used) cache for processing items Values can be registered and the cached cleaned so that it
  * doesn't go above a given size. The oldest elements are removed first.
  */
class LruCache[A](maxSize: Int, systemTime: SystemTime = JavaSystemTime):
  private var values: Map[A, Long] = Map.empty

  /** Checks if a value has already been processed; if not immediately adds it to the cache. If it has been processed,
    * refresh its timestamp.
    * @return
    *   the processed status
    */
  def register(value: A): Operation[ProcessedStatus] =
    Operation.delayed {
      this.synchronized:
        val alreadyProcessed = values.contains(value)
        // refresh the timestamp even if the params were already registered
        values += value -> systemTime.nanoTime
        val status = if alreadyProcessed then ProcessedStatus.AlreadyProcessed else ProcessedStatus.ToProcess
        while values.size > maxSize do values -= values.minBy(_._2)._1
        status
    }

  /** Return the number of elements in the cache */
  def size: Int =
    values.size

  /** Return the timestamp for the oldest element */
  def oldestTimestamp: Long =
    values.minBy(_._2)._2

/** This enum describes the status of an item in the LruCache */
enum ProcessedStatus:
  case AlreadyProcessed
  case ToProcess
