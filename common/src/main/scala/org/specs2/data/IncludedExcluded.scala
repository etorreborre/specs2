package org.specs2
package data

import text.Trim._
/**
 * This trait provides a keep function which will determine if a element T must be kept with regards to:
 *
 *  - a list of include tags
 *  - a list of exclude tags
 *  - a matchFunction which say if the element match a given tag
 *
 *  The element is kept if it is included and not excluded
 *
 *   - it is included if the include list is empty or the element matches an including tag
 *   - it is excluded if the exclude list is not empty and the element matches an excluding tag
 *
 */
trait IncludedExcluded[T] {
  def include: Seq[String]
  def exclude: Seq[String]

  def keepFunction: (T, Seq[String]) => Boolean
  def containFunction: (T, Seq[String]) => Boolean = keepFunction

  def keep(t: T): Boolean     = isIncluded(t)    && !isExcluded(t)
  def contain(t: T): Boolean  = isIncludedTag(t) && !isExcludedTag(t)

  def isIncluded(t: T) = include.isEmpty  || keepFunction(t, include)
  def isExcluded(t: T) = !exclude.isEmpty && keepFunction(t, exclude)

  def isIncludedTag(t: T) = include.isEmpty  || containFunction(t, include)
  def isExcludedTag(t: T) = !exclude.isEmpty && containFunction(t, exclude)
}

/**
 * specialization of the IncludedExcluded trait for string separated tags
 *
 * 2 tags t1, t2 separated by a "," means that t1 OR t2 must be included (/excluded)
 * 2 tags t1, t2 separated by a "&&" means that t1 AND t2 must be included (/excluded)
 */
case class SeparatedTags(included: String, excluded: String, orSeparator: String = ",", andSeparator: String = "&&") extends IncludedExcluded[Seq[String]] {
  val include = included.splitTrim(orSeparator)
  val exclude = excluded.splitTrim(orSeparator)

  val keepFunction = (n: Seq[String], tags: Seq[String]) => {
    tags.exists(wanted => wanted.splitTrim(andSeparator).forall(n.map(_.trim).contains))
  }

  override val containFunction = (n: Seq[String], tags: Seq[String]) => {
    tags.exists(wanted => wanted.splitTrim(andSeparator).exists(n.map(_.trim).contains))
  }
}
