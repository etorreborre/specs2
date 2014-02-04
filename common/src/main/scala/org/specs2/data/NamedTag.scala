package org.specs2
package data

import main.Arguments
import scalaz.Monoid

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

  override def toString = s"Tag(${names.mkString(",")})"
}

trait IncludeExcludeTag extends NamedTag { outer =>
  /** @return true if the element tagged with this must be kept */
  def keep(args: Arguments, names: Seq[String]): Boolean = SeparatedTags(args.include, args.exclude).keep(names)
}

object AlwaysTag extends NamedTag {
  def keep(args: Arguments, names: Seq[String]) = true
  def names = Seq("specs2.internal.always")
}

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

