package org.specs2.matcher
package describe

import org.specs2.Specification

import scala.util.{Failure, Try}

class DiffableSpec extends Specification { def is = s2"""

Compare result
==============
  primitives
    two identical ints should return PrimitiveIdentical         ${ Diffable.diff(1, 1) `must` ===(PrimitiveIdentical(1)) }
    two identical strings should return PrimitiveIdentical      ${ Diffable.diff("s", "s") `must` ===(PrimitiveIdentical("s")) }
    two different ints should return PrimitiveDifference        ${ Diffable.diff(1, 2) `must` ===(PrimitiveDifference(1, 2)) }
    two different strings should return PrimitiveDifference     ${ Diffable.diff("1", "2") `must` ===(PrimitiveDifference("1", "2")) }
    two different booleans should return PrimitiveDifference    ${ Diffable.diff(true, false) `must` ===(PrimitiveDifference(true, false)) }
    two different floats should return PrimitiveDifference      ${ Diffable.diff(0.5f, 0.6f) `must` ===(PrimitiveDifference(0.5f, 0.6f)) }
    two different longs should return PrimitiveDifference       ${ Diffable.diff(1l, 2l) `must` ===(PrimitiveDifference(1l, 2l)) }
    two different doubles should return PrimitiveDifference     ${ Diffable.diff(0.5d, 0.6d) `must` ===(PrimitiveDifference(0.5d, 0.6d)) }

  Exceptions
    two identical exceptions will return ThrowableIdentical                 ${ Diffable.diff(ex, ex) `must` ===(ThrowableIdentical(ex)) }
    two exceptions with a != message return ThrowableDifferentMessage       ${ Diffable.diff(ex, ex2) `must` ===(ThrowableDifferentMessage(PrimitiveDifference("m1", "m2"))) }
    two exceptions with a != stacktrace return ThrowableDifferentStackTrace ${ Diffable.diff(ex3, ex4) `must` ===(ThrowableDifferentStackTrace(LinesComparisonResult(List(se), List(se2)))) }

    two identical StackTraceElement return StackElementIdentical ${ Diffable.diff(se, se) `must` ===(StackElementIdentical(se)) }
    two different StackTraceElement return StackElementDifferent ${ Diffable.diff(se, se2) `must` ===(StackElementDifferent(className = PrimitiveDifference("class", "class1"),
                                                                                                                                     methodName = PrimitiveDifference("method", "method1"),
                                                                                                                                     fileName = Some(PrimitiveDifference("filename", "filename1")),
                                                                                                                                     lineNumber = PrimitiveDifference(666, 777))) }

  Fallback
  =========

  when giving different type comparison will fallback to default  ${ Diffable.diff(1, 1 : Any) `must` ===(OtherIdentical(1)) }
  two different case classes will return OtherClassDifferent      ${ Diffable.diff(Cat(), Dog()) `must` ===(OtherDifferent(Cat(), Dog())) }
  compare two nulls will return OtherIdentical                    ${ Diffable.diff(null : Any, null : Any) `must` ===(OtherIdentical(null)) }


  Maps
  ====
    equal maps should return MapIdentical                       ${ Diffable.diff(Map("x" -> "y"), Map("x" -> "y")) `must` ===(MapIdentical(Map("x" -> "y"))) }
    different map should show which elements were changed       ${ Diffable.diff(Map("x" -> "y"), Map("x" -> "z")) `must` ===(MapDifference(Seq.empty, changed = Seq("x" -> PrimitiveDifference("y", "z")), Seq.empty, Seq.empty)) }
    different map should show which elements were removed       ${ Diffable.diff(Map("x" -> "y"), Map.empty[String, String]) `must` ===(MapDifference(Seq.empty, Seq.empty, Seq.empty, removed = Seq("x" -> "y"))) }
    different map should show which elements were added         ${ Diffable.diff(Map.empty[String, String], Map("y" -> "z")) `must` ===(MapDifference(Seq.empty, Seq.empty, added = Seq("y" -> "z"), Seq.empty)) }
    different map should compose all changes                    $m1

  Set
  ===
    equal sets should return SetIdentical                         ${ Diffable.diff(Set("a", "b"), Set("a", "b")) `must` ===(SetIdentical(Set("a", "b")))}
    different sets should show which elements were added/removed  ${ Diffable.diff(Set("a", "b"), Set("a", "c")) `must` ===(SetDifference(Seq("a"),Seq("c"),Seq("b")))}
    different sets should show which elements were added          ${ Diffable.diff(Set("a", "b"), Set("a", "b", "c")) `must` ===(SetDifference(Seq("a","b"),Seq("c"),Seq.empty))}
    different sets should show which elements were removed        ${ Diffable.diff(Set("a", "b", "c"), Set("a", "b")) `must` ===(SetDifference(Seq("a", "b"),Seq.empty,Seq("c")))}
    Sets with added/removed and null values should work           ${ Diffable.diff(Set("a", null, "b"), Set("a", null, "c")) `must` ===(SetDifference(Seq("a", null),Seq("c"),Seq("b")))}'

  Seq (lines)
  ==========
    A Seq different should a return LineComparisonResult          ${ Diffable.diff(Seq("a", "b"),Seq("a", "b")) `must` ===(LinesComparisonResult(List("a", "b"), List("a", "b"))) }

  Seq (diff)
  ==========
    equal Seqs should return SeqIdentical                        ${ Diffable.diff(Seq("a", "b"),Seq("a", "b"))(using seqDiffable) `must` ===(SeqIdentical(Seq("a", "b"))) }
    different Seqs should show which elements were changed       ${ Diffable.diff(Seq("a"), Seq("b"))(using seqDiffable) `must` ===(SeqDifference(Seq(PrimitiveDifference("a", "b")), Seq.empty, Seq.empty)) }
    different Seqs should show which elements were added         ${ Diffable.diff(Seq("a"), Seq("a", "b"))(using seqDiffable) `must` ===(SeqDifference(Seq(PrimitiveIdentical("a")), Seq("b"), Seq.empty)) }
    different Seqs should show which elements were removed       ${ Diffable.diff(Seq("a", "b"), Seq("a"))(using seqDiffable) `must` ===(SeqDifference(Seq(PrimitiveIdentical("a")), Seq.empty, Seq("b"))) }
    different Seqs should show changed and added with null       ${ Diffable.diff(Seq("a"), Seq(null))(using seqDiffable) `must` ===(SeqDifference(Seq(PrimitiveDifference("a", null)), Seq.empty, Seq.empty)) }
    be able to compare Seq[Any] ${
      Diffable.diff(Seq("a", 1, 2l, 3.3d, 4.4f), Seq("b", 2, 3l, 4.4d, 5.5f))(using seqDiffable) `must` ===(
        SeqDifference(
          Seq(OtherDifferent("a", "b"),
          OtherDifferent(1, 2),
          OtherDifferent(2l, 3l),
          OtherDifferent(3.3d, 4.4d),
          OtherDifferent(4.4f, 5.5f)),
          Seq.empty, Seq.empty))
    }

  Arrays
  ======
    similar arrays will return similar iterable                   ${ Diffable.diff(Array(1, 2, 3), Array(1, 2, 3)) `must` ===(ArrayIdentical(Seq(1, 2, 3))) }
    different arrays should show which elements were changed      ${ Diffable.diff(Array(1), Array(2)) `must` ===(ArrayDifference(Seq(PrimitiveDifference(1, 2)), Seq.empty, Seq.empty)) }
    different arrays should show which elements were removed      ${ Diffable.diff(Array(1, 2), Array.empty[Int]) `must` ===(ArrayDifference(Seq.empty, Seq.empty, Seq(1, 2))) }
    different arrays should show which elements were added        ${ Diffable.diff(Array.empty[Int], Array(1, 2)) `must` ===(ArrayDifference(Seq.empty, Seq(1, 2), Seq.empty))}
    different arrays should compose all changes                   ${ Diffable.diff(Array(1, 2), Array(1, 9, 3)) `must` ===(ArrayDifference(Seq(PrimitiveIdentical(1), PrimitiveDifference(2, 9)), Seq(3), Seq.empty)) }

  Scala Objects: Option
  =====================

    similar option will return OptionIdentical                    ${ Diffable.diff(Option("abc"), Option("abc")) `must` ===(OptionIdentical(Some(PrimitiveIdentical("abc")))) }
    Some and None will return OptionDifferent                     ${ Diffable.diff(Option("abc"), None) `must` ===(OptionTypeDifferent(isActualSome = true, isExpectedSome = false)) }
    None and Some will return OptionDifferent                     ${ Diffable.diff(None, Option("abc")) `must` ===(OptionTypeDifferent(isActualSome = false, isExpectedSome = true)) }
    Some(x) and Some(y) will return OptionDifferent with result   ${ Diffable.diff(Option("abc"), Option("def")) `must` ===(OptionDifferent(PrimitiveDifference("abc", "def"))) }
    two None will return OptionIdentical                          ${ Diffable.diff(None, None) `must` ===(OptionIdentical(None)) }
    two (None: Option[String]) will return OptionIdentical        ${ Diffable.diff((None: Option[String]), (None: Option[String])) `must` ===(OptionIdentical(None)) }
    two identical Some will return OptionIdentical                ${ Diffable.diff(Some("abc"), Some("abc")) `must` ===(OptionIdentical(Some(PrimitiveIdentical("abc")))) }



    similar Either Right will return EitherIdentical  ${ Diffable.diff(Right[String, String]("abc"), Right[String, String]("abc")) `must` ===(EitherIdentical(PrimitiveIdentical("abc"), isRight = true))  }
    similar Either Left will return EitherIdentical   ${ Diffable.diff(Left[String, String]("abc"), Left[String, String]("abc")) `must` ===(EitherIdentical(PrimitiveIdentical("abc"), isRight = false))  }
    different Right will return EitherDifferent       ${ Diffable.diff(Right[String, String]("abc"), Right[String, String]("def")) `must` ===(EitherDifferent(PrimitiveDifference("abc", "def"), isRight = true))  }
    different Left will return EitherDifferent        ${ Diffable.diff(Left[String, String]("abc"), Left[String, String]("def")) `must` ===(EitherDifferent(PrimitiveDifference("abc", "def"), isRight = false))  }
    mixed Left and Right return EitherTypeDifferent   ${ Diffable.diff(Left[String, String]("abc"), Right[String, String]("def")) `must` ===(EitherTypeDifferent(isActualRight = false))  }
    mixed Left and Right return EitherTypeDifferent   ${ Diffable.diff(Right[String, String]("def"), Left[String, String]("abc")) `must` ===(EitherTypeDifferent(isActualRight = true))  }


  Scala Objects: Try
  ==================

  Similar Successful Try object will return TryIdentical        ${ Diffable.diff(Try("abc"), Try("abc")) `must` ===(TryIdentical("abc", isSuccess = true)) }
  Different Successful Try object will return TryDifferent      ${ Diffable.diff(Try("abc"), Try("def")) `must` ===(TryDifferent(PrimitiveDifference("abc", "def"), isSuccess = true)) }

  Similar Failure Try object will return TryIdentical           ${ Diffable.diff(Failure[RuntimeException](ex), Failure[RuntimeException](ex)) `must` ===(TryIdentical(ex, isSuccess = false)) }
  Different Failure Try object will return TryIdentical         ${ Diffable.diff(Failure[RuntimeException](ex), Failure[RuntimeException](ex2)) `must` ===(TryDifferent(Diffable.diff(ex, ex2), isSuccess = false)) }

  Comparing success with failure will return type difference    ${ Diffable.diff(Try("abc"), Failure[String](ex2)) `must` ===(TryTypeDifferent(isActualSuccess = true)) }
  Comparing success with failure will return type difference    ${ Diffable.diff(Failure[String](ex2), Try("abc")) `must` ===(TryTypeDifferent(isActualSuccess = false)) }
  Comparing failure with success will return type difference    ${ Diffable.diff(Try("abc"), Failure[String](ex2)) `must` ===(TryTypeDifferent(isActualSuccess = true)) }
  todo: handle non explicit types for Failure !!!

"""

  sealed case class ExampleFailure(message: String, stacktrace: List[StackTraceElement] = Nil) extends RuntimeException(message):
    override def getStackTrace = stacktrace.toArray

  val se = new StackTraceElement("class", "method", "filename", 666)
  val se2 = new StackTraceElement("class1", "method1", "filename1", 777)

  val ex = ExampleFailure("m1")
  val ex2 = ExampleFailure("m2")
  val ex3 = ExampleFailure("m", List(se))
  val ex4 = ExampleFailure("m", List(se2))

  def m1 =
    Diffable.diff(Map("a" -> "b", "c" -> "d", "e" -> "f"),
      Map("a" -> "b", "c" -> "x", "g" -> "h")) `must` ===(
      MapDifference(same    = Seq("a" -> "b"),
                    changed = Seq("c" -> PrimitiveDifference("d", "x")),
                    added   = Seq("g" -> "h"),
                    removed = Seq("e" -> "f")))

  def seqDiffable[T : Diffable]: SeqDiffable[T] =
    new SeqDiffable[T]
}


sealed trait Animal
case class Cat() extends Animal
case class Dog() extends Animal
