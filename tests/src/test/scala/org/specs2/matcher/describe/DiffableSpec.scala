package org.specs2.matcher
package describe

import org.specs2.Spec
import org.specs2.matcher._

import scala.util.{Failure, Try}
import CaseClassDiffs._

class DiffableSpec extends Spec { def is = s2"""

Compare result
==============
  primitives
    two identical ints should return PrimitiveIdentical         ${ Diffable.diff(1, 1) must_== PrimitiveIdentical(1) }
    two identical strings should return PrimitiveIdentical      ${ Diffable.diff("s", "s") must_== PrimitiveIdentical("s") }
    two different ints should return PrimitiveDifference        ${ Diffable.diff(1, 2) must_== PrimitiveDifference(1, 2) }
    two different strings should return PrimitiveDifference     ${ Diffable.diff("1", "2") must_=== PrimitiveDifference("1", "2") }
    two different booleans should return PrimitiveDifference    ${ Diffable.diff(true, false) must_=== PrimitiveDifference(true, false) }
    two different floats should return PrimitiveDifference      ${ Diffable.diff(0.5f, 0.6f) must_=== PrimitiveDifference(0.5f, 0.6f) }
    two different longs should return PrimitiveDifference       ${ Diffable.diff(1l, 2l) must_== PrimitiveDifference(1l, 2l) }
    two different doubles should return PrimitiveDifference     ${ Diffable.diff(0.5d, 0.6d) must_=== PrimitiveDifference(0.5d, 0.6d) }

  Exceptions
    two identical exceptions will return ThrowableIdentical     ${ Diffable.diff(ex, ex) must_=== ThrowableIdentical(ex) }
    two different exceptions will return ThrowableDifferent     ${ Diffable.diff(ex, ex2) must haveOneDifferenceOf(Diffable.diff(ex.getStackTrace.head, ex2.getStackTrace.head)) }

    two identical StackTraceElement return StackElementIdentical ${ Diffable.diff(se, se) must_=== StackElementIdentical(se) }
    two different StackTraceElement return StackElementDifferent ${ Diffable.diff(se, se2) must_=== StackElementDifferent(className = PrimitiveDifference("class", "class1"),
                                                                                                                                     methodName = PrimitiveDifference("method", "method1"),
                                                                                                                                     fileName = Some(PrimitiveDifference("filename", "filename1")),
                                                                                                                                     lineNumber = PrimitiveDifference(666, 777)) }

  Fallback
  =========

  when giving different type comparison will fallback to default  ${ Diffable.diff(1, 1 : Any) must_=== OtherIdentical(1) }
  two different case classes will return OtherClassDifferent      ${ Diffable.diff(Cat(), Dog()) must_== OtherDifferent(Cat(), Dog()) }
  compare two nulls will return OtherIdentical                    ${ Diffable.diff(null : Any, null : Any) must_== OtherIdentical(null) }


  Maps
  ====
    equal maps should return MapIdentical                       ${ Diffable.diff(Map("x" -> "y"), Map("x" -> "y")) must_== MapIdentical(Map("x" -> "y")) }
    different map should show which elements were changed       ${ Diffable.diff(Map("x" -> "y"), Map("x" -> "z")) must_== MapDifference(Seq.empty, changed = Seq("x" -> PrimitiveDifference("y", "z")), Seq.empty, Seq.empty) }
    different map should show which elements were removed       ${ Diffable.diff(Map("x" -> "y"), Map.empty[String, String]) must_==  MapDifference(Seq.empty, Seq.empty, Seq.empty, removed = Seq("x" -> "y"))}
    different map should show which elements were added         ${ Diffable.diff(Map.empty[String, String], Map("y" -> "z")) must_== MapDifference(Seq.empty, Seq.empty, added = Seq("y" -> "z"), Seq.empty)}
    different map should compose all changes                    $m1

   Set
   ===
    equal sets should return SetIdentical                         ${ Diffable.diff(Set("a", "b"), Set("a", "b")) must_== SetIdentical(Set("a", "b"))}
    different sets should show which elements were added/removed  ${ Diffable.diff(Set("a", "b"), Set("a", "c")) must_== SetDifference(Seq("a"),Seq("c"),Seq("b"))}
    different sets should show which elements were added          ${ Diffable.diff(Set("a", "b"), Set("a", "b", "c")) must_== SetDifference(Seq("a","b"),Seq("c"),Seq.empty)}
    different sets should show which elements were removed        ${ Diffable.diff(Set("a", "b", "c"), Set("a", "b")) must_== SetDifference(Seq("a", "b"),Seq.empty,Seq("c"))}
    Sets with added/removed and null values should work           ${ Diffable.diff(Set("a", null, "b"), Set("a", null, "c")) must_== SetDifference(Seq("a", null),Seq("c"),Seq("b"))}'

  Seq (diff)
  ==========
    equal Seqs should return SeqIdentical                        ${ Diffable.diff(Seq("a", "b"),Seq("a", "b")) must_=== SeqIdentical(Seq("a", "b")) }
    different Seqs should show which elements were changed       ${ Diffable.diff(Seq("a"), Seq("b")) must_=== SeqDifference(Seq(PrimitiveDifference("a", "b")), Seq.empty, Seq.empty) }
    different Seqs should show which elements were added         ${ Diffable.diff(Seq("a"), Seq("a", "b")) must_=== SeqDifference(Seq(PrimitiveIdentical("a")), Seq("b"), Seq.empty) }
    different Seqs should show which elements were removed       ${ Diffable.diff(Seq("a", "b"), Seq("a")) must_=== SeqDifference(Seq(PrimitiveIdentical("a")), Seq.empty, Seq("b")) }
    different Seqs should show changed and added with null       ${ Diffable.diff(Seq("a"), Seq(null)) must_=== SeqDifference(Seq(PrimitiveDifference("a", null)), Seq.empty, Seq.empty) }
    be able to compare Seq[Any]                                  ${ Diffable.diff(Seq("a", 1, 2l, 3.3d, 4.4f), Seq("b", 2, 3l, 4.4d, 5.5f)) must_===
                                                                        SeqDifference(Seq(OtherDifferent("a", "b"), OtherDifferent(1, 2), OtherDifferent(2l, 3l), OtherDifferent(3.3d, 4.4d), OtherDifferent(4.4f, 5.5f)), Seq.empty, Seq.empty) }

  Arrays
  ======
    similar arrays will return similar iterable                   ${ Diffable.diff(Array(1, 2, 3), Array(1, 2, 3)) must_== ArrayIdentical(Seq(1, 2, 3)) }
    different arrays should show which elements were changed      ${ Diffable.diff(Array(1), Array(2)) must_== ArrayDifference(Seq(PrimitiveDifference(1, 2)), Seq.empty, Seq.empty) }
    different arrays should show which elements were removed      ${ Diffable.diff(Array(1, 2), Array.empty[Int]) must_==  ArrayDifference(Seq.empty, Seq.empty, Seq(1, 2)) }
    different arrays should show which elements were added        ${ Diffable.diff(Array.empty[Int], Array(1, 2)) must_== ArrayDifference(Seq.empty, Seq(1, 2), Seq.empty)}
    different arrays should compose all changes                   ${ Diffable.diff(Array(1, 2), Array(1, 9, 3)) must_== ArrayDifference(Seq(PrimitiveIdentical(1), PrimitiveDifference(2, 9)), Seq(3), Seq.empty)}

  Scala Objects: Option
  =====================

    similar option will return OptionIdentical                    ${ Diffable.diff(Option("abc"), Option("abc")) must_=== OptionIdentical(Some(PrimitiveIdentical("abc"))) }
    Some and None will return OptionDifferent                     ${ Diffable.diff(Option("abc"), None) must_=== OptionTypeDifferent(isActualSome = true, isExpectedSome = false) }
    None and Some will return OptionDifferent                     ${ Diffable.diff(None, Option("abc")) must_=== OptionTypeDifferent(isActualSome = false, isExpectedSome = true) }
    Some(x) and Some(y) will return OptionDifferent with result   ${ Diffable.diff(Option("abc"), Option("def")) must_=== OptionDifferent(PrimitiveDifference("abc", "def")) }
    two None will return OptionIdentical                          ${ Diffable.diff(None, None) must_=== OptionIdentical(None) }
    two identical Some will return OptionIdentical                ${ Diffable.diff(Some("abc"), Some("abc")) must_=== OptionIdentical(Some(PrimitiveIdentical("abc"))) }



    similar Either Right will return EitherIdentical  ${ Diffable.diff(Right[String, String]("abc"), Right[String, String]("abc")) must_=== EitherIdentical(PrimitiveIdentical("abc"), isRight = true)  }
    similar Either Left will return EitherIdentical   ${ Diffable.diff(Left[String, String]("abc"), Left[String, String]("abc")) must_=== EitherIdentical(PrimitiveIdentical("abc"), isRight = false)  }
    different Right will return EitherDifferent       ${ Diffable.diff(Right[String, String]("abc"), Right[String, String]("def")) must_=== EitherDifferent(PrimitiveDifference("abc", "def"), isRight = true)  }
    different Left will return EitherDifferent        ${ Diffable.diff(Left[String, String]("abc"), Left[String, String]("def")) must_=== EitherDifferent(PrimitiveDifference("abc", "def"), isRight = false)  }
    mixed Left and Right return EitherTypeDifferent   ${ Diffable.diff(Left[String, String]("abc"), Right[String, String]("def")) must_=== EitherTypeDifferent(isActualRight = false)  }
    mixed Left and Right return EitherTypeDifferent   ${ Diffable.diff(Right[String, String]("def"), Left[String, String]("abc")) must_=== EitherTypeDifferent(isActualRight = true)  }


  Scala Objects: Try
  ==================

  Similar Successful Try object will return TryIdentical        ${ Diffable.diff(Try("abc"), Try("abc")) must_=== TryIdentical("abc", isSuccess = true) }
  Different Successful Try object will return TryDifferent      ${ Diffable.diff(Try("abc"), Try("def")) must_=== TryDifferent(PrimitiveDifference("abc", "def"), isSuccess = true) }

  Similar Failure Try object will return TryIdentical           ${ Diffable.diff(Failure[RuntimeException](ex), Failure[RuntimeException](ex)) must_=== TryIdentical(ex, isSuccess = false) }
  Different Failure Try object will return TryIdentical         ${ Diffable.diff(Failure[RuntimeException](ex), Failure[RuntimeException](ex2)) must_=== TryDifferent(Diffable.diff(ex, ex2), isSuccess = false) }

  Comparing success with failure will return type difference    ${ Diffable.diff(Try("abc"), Failure[String](ex2)) must_=== TryTypeDifferent(isActualSuccess = true) }
  Comparing success with failure will return type difference    ${ Diffable.diff(Failure[String](ex2), Try("abc")) must_=== TryTypeDifferent(isActualSuccess = false) }
  Comparing failure with success will return type difference    ${ Diffable.diff(Try("abc"), Failure[String](ex2)) must_=== TryTypeDifferent(isActualSuccess = true) }
  todo: handle non explicit types for Failure !!!


    We need to support different type compare
"""

  val ex = new RuntimeException
  val ex2 = new RuntimeException

  val se = new StackTraceElement("class", "method", "filename", 666)
  val se2 = new StackTraceElement("class1", "method1", "filename1", 777)


  def m1 = {
    Diffable.diff(Map("a" -> "b", "c" -> "d", "e" -> "f"),
      Map("a" -> "b", "c" -> "x", "g" -> "h")) must_==
      MapDifference(same    = Seq("a" -> "b"),
                    changed = Seq("c" -> PrimitiveDifference("d", "x")),
                    added   = Seq("g" -> "h"),
                    removed = Seq("e" -> "f"))
  }



  trait TraversableWithNoDefinedForeach[T] extends Traversable[T] {
    def foreach[U](f: T => U): Unit = {
      sys.error("foreach is not defined on this traversable but toString is")
    }
  }

  def haveOneDifferenceOf(diff: ComparisonResult): Matcher[ComparisonResult] = new Matcher[ComparisonResult] {
    def apply[S <: ComparisonResult](t: Expectable[S]) =
      t.value match {
        case v: ThrowableDifferent if v.added.nonEmpty || v.removed.nonEmpty => failure("Exception contained added or removed lines", t)
        case v: ThrowableDifferent if v.result.count( _.isInstanceOf[StackElementDifferent] ) > 1 => failure("Exception contained more than once difference in stacktrace", t)
        case v: ThrowableDifferent if v.result.headOption.exists(_ == diff) => success("ok", t)
        case v: ThrowableDifferent if !v.result.headOption.exists(_ == diff) => failure(s"expected: $diff instead got: ${v.result.head}", t)
        case e => failure(s"result is of wrong type, should be ThrowableDifferent but was [${e.getClass.getSimpleName}]", t)
      }
  }

}


sealed trait Animal
case class Cat() extends Animal
case class Dog() extends Animal

