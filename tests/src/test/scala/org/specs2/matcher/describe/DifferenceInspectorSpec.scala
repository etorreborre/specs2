package org.specs2.matcher.describe

import org.specs2.Spec
import org.specs2.matcher.{Expectable, Matcher}

import scala.util.{Failure, Try}

class DifferenceInspectorSpec extends Spec { def is = s2"""

Compare result
==============
  primitives
    two identical ints should return PrimitiveIdentical         ${ differenceInspector.diff(1, 1) must_== PrimitiveIdentical(1) }
    two identical strings should return PrimitiveIdentical      ${ differenceInspector.diff("s", "s") must_== PrimitiveIdentical("s") }
    two different ints should return PrimitiveDifference        ${ differenceInspector.diff(1, 2) must_== PrimitiveDifference(1, 2) }
    two different strings should return PrimitiveDifference     ${ differenceInspector.diff("1", "2") must_=== PrimitiveDifference("1", "2") }
    two different booleans should return PrimitiveDifference    ${ differenceInspector.diff(true, false) must_=== PrimitiveDifference(true, false) }
    two different floats should return PrimitiveDifference      ${ differenceInspector.diff(0.5f, 0.6f) must_=== PrimitiveDifference(0.5f, 0.6f) }
    two different longs should return PrimitiveDifference       ${ differenceInspector.diff(1l, 2l) must_== PrimitiveDifference(1l, 2l) }
    two different doubles should return PrimitiveDifference     ${ differenceInspector.diff(0.5d, 0.6d) must_=== PrimitiveDifference(0.5d, 0.6d) }

  Exceptions
    two identical exceptions will return ThrowableIdentical     ${ differenceInspector.diff(ex, ex) must_=== ThrowableIdentical(ex) }
    two different exceptions will return ThrowableDifferent     ${ differenceInspector.diff(ex, ex2) must haveOneDifferenceOf(differenceInspector.diff(ex.getStackTrace.head, ex2.getStackTrace.head)) }

    two identical StackTraceElement return StackElementIdentical ${ differenceInspector.diff(se, se) must_=== StackElementIdentical(se) }
    two different StackTraceElement return StackElementDifferent ${ differenceInspector.diff(se, se2) must_=== StackElementDifferent(className = PrimitiveDifference("class", "class1"),
                                                                                                                                     methodName = PrimitiveDifference("method", "method1"),
                                                                                                                                     fileName = Some(PrimitiveDifference("filename", "filename1")),
                                                                                                                                     lineNumber = PrimitiveDifference(666, 777)) }

  Fallback
  =========

  when giving different type comparison will fallback to default  ${ differenceInspector.diff(1, 1 : Any) must_=== OtherIdentical(1) }
  two different case classes will return OtherClassDifferent      ${ differenceInspector.diff(Cat(), Dog()) must_== OtherDifferent(Cat(), Dog()) }
  compare two nulls will return OtherIdentical                    ${ differenceInspector.diff(null : Any, null : Any) must_== OtherIdentical(null) }


  Maps
  ====
    equal maps should return MapIdentical                       ${ differenceInspector.diff(Map("x" -> "y"), Map("x" -> "y")) must_== MapIdentical(Map("x" -> "y")) }
    different map should show which elements were changed       ${ differenceInspector.diff(Map("x" -> "y"), Map("x" -> "z")) must_== MapDifference(Seq.empty, changed = Seq("x" -> PrimitiveDifference("y", "z")), Seq.empty, Seq.empty) }
    different map should show which elements were removed       ${ differenceInspector.diff(Map("x" -> "y"), Map.empty[String, String]) must_==  MapDifference(Seq.empty, Seq.empty, Seq.empty, removed = Seq("x" -> "y"))}
    different map should show which elements were added         ${ differenceInspector.diff(Map.empty[String, String], Map("y" -> "z")) must_== MapDifference(Seq.empty, Seq.empty, added = Seq("y" -> "z"), Seq.empty)}
    different map should compose all changes                    $m1

   Set
   ===
    equal sets should return SetIdentical                         ${ differenceInspector.diff(Set("a", "b"), Set("a", "b")) must_== SetIdentical(Set("a", "b"))}
    different sets should show which elements were added/removed  ${ differenceInspector.diff(Set("a", "b"), Set("a", "c")) must_== SetDifference(Seq("a"),Seq("c"),Seq("b"))}
    different sets should show which elements were added          ${ differenceInspector.diff(Set("a", "b"), Set("a", "b", "c")) must_== SetDifference(Seq("a","b"),Seq("c"),Seq.empty)}
    different sets should show which elements were removed        ${ differenceInspector.diff(Set("a", "b", "c"), Set("a", "b")) must_== SetDifference(Seq("a", "b"),Seq.empty,Seq("c"))}
    Sets with added/removed and null values should work           ${ differenceInspector.diff(Set("a", null, "b"), Set("a", null, "c")) must_== SetDifference(Seq("a", null),Seq("c"),Seq("b"))}'

  Seq (diff)
  ==========
    equal Seqs should return SeqIdentical                        ${ differenceInspector.diff(Seq("a", "b"),Seq("a", "b")) must_== SeqIdentical(Seq("a", "b"))}
    different Seqs should show which elements were changed       ${ differenceInspector.diff(Seq("a"), Seq("b")) must_== SeqDifference(Seq(PrimitiveDifference("a", "b")), Seq.empty, Seq.empty)}
    different Seqs should show which elements were added         ${ differenceInspector.diff(Seq("a"), Seq("a", "b")) must_== SeqDifference(Seq(PrimitiveIdentical("a")), Seq("b"), Seq.empty)}
    different Seqs should show which elements were removed       ${ differenceInspector.diff(Seq("a", "b"), Seq("a")) must_== SeqDifference(Seq(PrimitiveIdentical("a")), Seq.empty, Seq("b"))}
    different Seqs should show changed and added with null       ${ differenceInspector.diff(Seq("a"), Seq(null)) must_== SeqDifference(Seq(PrimitiveDifference("a", null)), Seq.empty, Seq.empty)}
    be able to compare Seq[Any]                                  ${differenceInspector.diff(Seq("a", 1, 2l, 3.3d, 4.4f), Seq("b", 2, 3l, 4.4d, 5.5f)) must_==
                                                                        SeqDifference(Seq(OtherDifferent("a", "b"), OtherDifferent(1, 2), OtherDifferent(2l, 3l), OtherDifferent(3.3d, 4.4d), OtherDifferent(4.4f, 5.5f)), Seq.empty, Seq.empty)}

  case classes
  ============
    case classes without any members will return CaseClassIdentical ${ differenceInspector.diff(EmptyCaseClass(), EmptyCaseClass()) must_== CaseClassIdentical("EmptyCaseClass") }
    two identical case classes will return CaseClassIdentical       ${ differenceInspector.diff(Goo("a", "b"), Goo("a", "b")) must_== CaseClassIdentical("Goo") }
    two different case classes will return CaseClassDifferent       ${ differenceInspector.diff(Goo(c = "a", d = "b"), Goo(c = "x", d = "b")) must_== CaseClassDifferent("Goo", Seq(CaseClassPropertyComparison("c", PrimitiveDifference("a", "x"), identical = false), CaseClassPropertyComparison("d", PrimitiveIdentical("b"), identical = true))) }



  Arrays
  ======
    similar arrays will return similar iterable                   ${ differenceInspector.diff(Array(1, 2, 3), Array(1, 2, 3)) must_== ArrayIdentical(Seq(1, 2, 3)) }
    different arrays should show which elements were changed      ${ differenceInspector.diff(Array(1), Array(2)) must_== ArrayDifference(Seq(PrimitiveDifference(1, 2)), Seq.empty, Seq.empty) }
    different arrays should show which elements were removed      ${ differenceInspector.diff(Array(1, 2), Array.empty[Int]) must_==  ArrayDifference(Seq.empty, Seq.empty, Seq(1, 2)) }
    different arrays should show which elements were added        ${ differenceInspector.diff(Array.empty[Int], Array(1, 2)) must_== ArrayDifference(Seq.empty, Seq(1, 2), Seq.empty)}
    different arrays should compose all changes                   ${ differenceInspector.diff(Array(1, 2), Array(1, 9, 3)) must_== ArrayDifference(Seq(PrimitiveIdentical(1), PrimitiveDifference(2, 9)), Seq(3), Seq.empty)}

  Scala Objects: Option
  =====================

    similar option will return OptionIdentical                    ${ differenceInspector.diff(Option("abc"), Option("abc")) must_=== OptionIdentical(Some(PrimitiveIdentical("abc"))) }
    Some and None will return OptionDifferent                     ${ differenceInspector.diff(Option("abc"), None) must_=== OptionTypeDifferent(isActualSome = true, isExpectedSome = false) }
    None and Some will return OptionDifferent                     ${ differenceInspector.diff(None, Option("abc")) must_=== OptionTypeDifferent(isActualSome = false, isExpectedSome = true) }
    Some(x) and Some(y) will return OptionDifferent with result   ${ differenceInspector.diff(Option("abc"), Option("def")) must_=== OptionDifferent(PrimitiveDifference("abc", "def")) }
    two None will return OptionIdentical                          ${ differenceInspector.diff(None, None) must_=== OptionIdentical(None) }
    two identical Some will return OptionIdentical                ${ differenceInspector.diff(Some("abc"), Some("abc")) must_=== OptionIdentical(Some(PrimitiveIdentical("abc"))) }


  Scala Objects: Either
  =====================

    similar Either Right will return EitherIdentical  ${ differenceInspector.diff(Right[String, String]("abc"), Right[String, String]("abc")) must_=== EitherIdentical(PrimitiveIdentical("abc"), isRight = true)  }
    similar Either Left will return EitherIdentical   ${ differenceInspector.diff(Left[String, String]("abc"), Left[String, String]("abc")) must_=== EitherIdentical(PrimitiveIdentical("abc"), isRight = false)  }
    different Right will return EitherDifferent       ${ differenceInspector.diff(Right[String, String]("abc"), Right[String, String]("def")) must_=== EitherDifferent(PrimitiveDifference("abc", "def"), isRight = true)  }
    different Left will return EitherDifferent        ${ differenceInspector.diff(Left[String, String]("abc"), Left[String, String]("def")) must_=== EitherDifferent(PrimitiveDifference("abc", "def"), isRight = false)  }
    mixed Left and Right return EitherTypeDifferent   ${ differenceInspector.diff(Left[String, String]("abc"), Right[String, String]("def")) must_=== EitherTypeDifferent(isActualRight = false)  }
    mixed Left and Right return EitherTypeDifferent   ${ differenceInspector.diff(Right[String, String]("def"), Left[String, String]("abc")) must_=== EitherTypeDifferent(isActualRight = true)  }

    Support Right without Left type information       ${ differenceInspector.diff(Right("abc"), Right("abc")) must_=== EitherIdentical(PrimitiveIdentical("abc"), isRight = true)  }
    Support Left without Right type information       ${ differenceInspector.diff(Left("abc"), Left("abc")) must_=== EitherIdentical(PrimitiveIdentical("abc"), isRight = false)  }



  Scala Objects: Try
  ==================

  Similar Successful Try object will return TryIdentical        ${ differenceInspector.diff(Try("abc"), Try("abc")) must_=== TryIdentical("abc", isSuccess = true) }
  Different Successful Try object will return TryDifferent      ${ differenceInspector.diff(Try("abc"), Try("def")) must_=== TryDifferent(PrimitiveDifference("abc", "def"), isSuccess = true) }

  Similar Failure Try object will return TryIdentical           ${ differenceInspector.diff(Failure[RuntimeException](ex), Failure[RuntimeException](ex)) must_=== TryIdentical(ex, isSuccess = false) }
  Different Failure Try object will return TryIdentical         ${ differenceInspector.diff(Failure[RuntimeException](ex), Failure[RuntimeException](ex2)) must_=== TryDifferent(differenceInspector.diff(ex, ex2), isSuccess = false) }
  Support failure with no type information                      ${ differenceInspector.diff(Failure(ex), Failure(ex2)) must_=== TryDifferent(differenceInspector.diff(ex, ex2), isSuccess = false) }

  Comparing success with failure will return type difference    ${ differenceInspector.diff(Try("abc"), Failure[String](ex2)) must_=== TryTypeDifferent(isActualSuccess = true) }
  Comparing success with failure will return type difference    ${ differenceInspector.diff(Failure[String](ex2), Try("abc")) must_=== TryTypeDifferent(isActualSuccess = false) }
  Comparing failure with success will return type difference    ${ differenceInspector.diff(Try("abc"), Failure[String](ex2)) must_=== TryTypeDifferent(isActualSuccess = true) }
  todoL: handle non explicit types for Failure !!!


    We need to support different type compare
"""

  val differenceInspector: DifferenceInspector = new DefaultDifferenceInspector

  val ex = new RuntimeException
  val ex2 = new RuntimeException

  val se = new StackTraceElement("class", "method", "filename", 666)
  val se2 = new StackTraceElement("class1", "method1", "filename1", 777)


  def m1 = {
    differenceInspector.diff(Map("a" -> "b", "c" -> "d", "e" -> "f"),
      Map("a" -> "b", "c" -> "x", "g" -> "h")) must_==
      MapDifference(identical = Seq("a" -> "b"),
                    changed = Seq("c" -> PrimitiveDifference("d", "x")),
                    added = Seq("g" -> "h"),
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
        case v: ThrowableDifferent if v.result.headOption.contains(diff) => success("ok", t)
        case v: ThrowableDifferent if !v.result.headOption.contains(diff) => failure(s"expected: $diff instead got: ${v.result.head}", t)
        case e => failure(s"result is of wrong type, should be ThrowableDifferent but was [${e.getClass.getSimpleName}]", t)
      }
  }

}

case class Foo(a: String, b: String, goo: Goo)

case class Goo(c: String, d: String)
case class EmptyCaseClass()


sealed trait Animal
case class Cat() extends Animal
case class Dog() extends Animal

