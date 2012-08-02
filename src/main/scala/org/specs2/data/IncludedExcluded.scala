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
  val include: Seq[String]
  val exclude: Seq[String]
  val matchFunction: (T, Seq[String]) => Boolean

  def keep(t: T): Boolean = isIncluded(t) && !isExcluded(t)

  def isIncluded(t: T) = include.isEmpty || matchFunction(t, include)
  def isExcluded(t: T) = !exclude.isEmpty && matchFunction(t, exclude)
}

/**
 * specialization of the IncludedExcluded trait for string separated tags
 *
 * 2 tags t1, t2 separated by a "," means that t1 OR t2 must be included (/excluded)
 * 2 tags t1, t2 separated by a "&&" means that t1 AND t2 must be included (/excluded)
 */
case class SeparatedTags(included: String, excluded: String, orSeparator: String = ",", andSeparator: String = "&&") extends IncludedExcluded[Seq[String]] {
  val matchFunction = (n: Seq[String], tags: Seq[String]) => {
    tags.exists(wanted => wanted.splitTrim(andSeparator).forall(n.map(_.trim).contains))
  }
  val include = included.splitTrim(orSeparator)
  val exclude = excluded.splitTrim(orSeparator)
}
