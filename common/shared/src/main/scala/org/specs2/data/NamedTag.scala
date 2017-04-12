package org.specs2
package data

import main.Arguments
import org.specs2.fp._

/**
 * A tag with names.
 *
 * Given a list of names and the current arguments this tag must be able to determine if what it is tagging must
 * be kept or not
 */
trait NamedTag { outer =>
  /** tagging names */
  def names: Seq[String]
  /** @return true if the element tagged with this must be kept in the context of a specific set of names */
  def keep(args: Arguments, names: Seq[String]): Boolean
  /** @return true if the element tagged with this must be kept */
  def keep(args: Arguments): Boolean = keep(args, names)

  /** @return a tag where both "keep" conditions apply and where new names are used for evaluating the "keep" condition */
  def overrideWith(other: NamedTag): NamedTag = new NamedTag {
    def keep(args: Arguments, names: Seq[String]) = outer.keep(args, names) && other.keep(args, names)
    def names = (outer.names ++ other.names).distinct
  }

  def removeNames(otherNames: Seq[String]): NamedTag =
    setNames(outer.names.diff(otherNames))

  def setNames(otherNames: Seq[String]): NamedTag = new NamedTag {
    def keep(args: Arguments, names: Seq[String]) = outer.keep(args, names)
    def names = otherNames.distinct
  }

  override def equals(o: Any) =
    o match {
      case t: NamedTag => names.distinct.toSet == t.names.distinct.toSet
      case _           => false
    }

  override def hashCode =
    names.hashCode

  override def toString = s"Tag(${names.mkString(",")})"
}

/**
 * An IncludeExcludeTag is defined with inclusion/exclusion patterns
 */
trait IncludeExcludeTag extends NamedTag { outer =>
  /** @return true if the element tagged with this must be kept */
  def keep(args: Arguments, names: Seq[String]): Boolean =
    SeparatedTags(args.include, args.exclude).keep(names)
}

/**
 * This tag will always keep its tagged element.
 *
 * It is used to keep setup/teardown behaviour in specification whether examples are tagged or not and
 * whatever is passed on the command line
 */
object AlwaysTag extends NamedTag {
  def keep(args: Arguments, names: Seq[String]) = true
  def names = Seq("specs2.internal.always")
}

/**
 * Similar to the AlwaysTag this tag is keeping elements only if there is no included tags
 */
object AlwaysWhenNoIncludeTag extends NamedTag {
  def keep(args: Arguments, names: Seq[String]) = args.include.isEmpty
  def names = Seq("specs2.internal.alwaysWhenNoInclude")
}

/** tags the next element */
case class Tag(names: String*) extends IncludeExcludeTag

object NamedTag {
  /**
   * define a very coarse Monoid for NamedTags where appending 2 NamedTags returns a Tag object
   * with both list of tags
   */
  implicit val NamedTagsAreMonoid = new Monoid[NamedTag] {
    val zero: NamedTag = AlwaysWhenNoIncludeTag
    def append(t1: NamedTag, t2: =>NamedTag): NamedTag =
      if (t1 == zero) t2
      else if (t2 == zero) t1
      else t1 overrideWith t2
  }
}

