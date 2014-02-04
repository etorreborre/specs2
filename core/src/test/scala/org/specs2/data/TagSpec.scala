package org.specs2
package data

import scalaz.{Tag => _}
import scalaz.syntax.semigroup._
import org.specs2.main.Arguments
import org.specs2.matcher.DataTables
import NamedTag._
import NamedTagsAreMonoid._

class TagSpec extends Specification with DataTables { def is = s2"""

  Tags (simple, named, section, custom) form a monoid so that:

    there is a zero tag                          $zeroTag
    the monoid is associative                    $associative
    adding 2 named tags uses both keep functions $add1

  It is possible to remove tags from a tag       $remove1

 """

  def zeroTag = allTags must contain { t: NamedTag =>
    allNames must contain { names: String =>
      val arguments = Arguments("include "+names)

      ((t |+| zero) ==== (zero |+| t)) and
      ((t |+| zero).keep(arguments) ==== (zero |+| t).keep(arguments))
    }.forall
  }.forall

  def associative = allTags must contain { t1: NamedTag =>
    allTags must contain { t2: NamedTag =>
      allTags must contain { t3: NamedTag =>
        allNames must contain { names: String =>
          val arguments = Arguments("include "+names)
          val values = (t1, t2, t3, names).toString

          (((t1 |+| t2) |+| t3)                             must_== (t1 |+| (t2 |+| t3))) and
          (((t1 |+| t2) |+| t3).keep(arguments).aka(values) must_== (t1 |+| (t2 |+| t3)).keep(arguments))
        }.forall
      }.forall
    }.forall
  }.forall

  def add1 = {
    "t1"     | "t2"  | "args"           | "result" |>
    tag1     ! tag2  ! "include 1"      ! true     |
    tag1     ! tag2  ! "include 2"      ! true     |
    tag1     ! tag1  ! "include 1&&2"   ! false    |
    tag1     ! tag2  ! "include 1&&2"   ! true     |
      { (t1: NamedTag, t2: NamedTag, args: String, result: Boolean) =>
      (t1 |+| t2).keep(Arguments(args)) must_== result
    }
  }

  def remove1 = Tag("1", "2", "3").removeNames(Seq("2")) must_== Tag("1", "3")

  val tag1: NamedTag = Tag("1")
  val tag2: NamedTag = Tag("2")

  val allTags = Seq(tag1, tag2, AlwaysTag, AlwaysWhenNoIncludeTag)

  val allNames =
    Seq("1", "2", "3", "1&&2")

}
