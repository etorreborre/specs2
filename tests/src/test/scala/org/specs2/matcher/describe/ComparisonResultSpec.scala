package org.specs2.matcher.describe

import java.io.{PrintWriter, StringWriter}

import org.specs2.Spec
import org.specs2.matcher.Hello

class ComparisonResultSpec extends Spec { def is = s2"""

    Primitive render:
    =================

      render primitive to toString of the value                     $p1
      render string to quoted string                                $p2
      render primitive difference to X != Y                         $p3
      render primitive String difference to quoted 'X' != 'Y'       $p4
      render null values                                            $p5

    Seq render:
    ===========

    identicate seq should print to string of the set                                        $seq1
    different seq should print identical values first                                       $seq2
    different seq should print changed values first                                         $seq3
    different seq should print added values with prefix added: a                            $seq4
    different seq should print removed values with prefix removed: b                        $seq5
    print order should be Seq(identical, added: added, removed: removed)                    $seq6

    Array render:
    =============

    identicate array should print to string of the set                                      $arr1
    different array should print identical values first                                     $arr2
    different array should print changed values first                                       $arr3
    different array should print added values with prefix added: a                          $arr4
    different array should print removed values with prefix removed: b                      $arr5
    print order should be Array(identical, added: added, removed: removed)                  $arr6


    Set render:
    ===========

    identicate set should print to string of the set                                        $set1
    different set should printe identical values first                                      $set2
    different set should printe added values with prefix added: a                           $set3
    different set should printe removed values with prefix removed: b                       $set4
    print order should be Set(identical, added: added, removed: removed)                    $set5

    Map render:
    ===========

    identicate map should print to string of the map                                        $m1
    different map should printe identical values first                                      $m2
    different map should printe non identical values second in format: x -> { y != z }      $m3
    different map should printe added values with prefix added: x -> y                      $m4
    different map should printe removed values with prefix removed: x -> y                  $m5
    print order should be Map(identical, changed, added: added, removed: removed)           $m6

    Option render:
    ==============

    identical option will return the type Some(...)   $o1
    identical option without value will None          $o2
    different option will render the different        $o3
    different option type Some(...) ==> None          $o4
    different option type None ==> Some(...)          $o5

    Either render:
    ==============

    identical Left will return the type Left(...)   $e1
    identical Left will return the type Right(...)  $e2
    different right render the different            $e3
    different left render the different             $e4
    different either type render Left ==> Right     $e5
    different either type render Right ==> Left     $e6

    Stacktrace render:
    ==================

    identical stack element is just to string of stack element              $se1
    different stack element will render all differences                     $se2
    different stack element without file name will print (Unknown Source)   $se3

    identical identical throwable will toString throwable                   $th1
    identical identical throwable will toString throwable                   $th2


    Try render:
    ===========

    identical success will return Success(...)                              ${ TryIdentical(1, isSuccess = true).render must_=== "Success(1)" }
    identical failure will return Failure(...)                              ${ TryIdentical(1, isSuccess = false).render must_=== "Failure(1)" }
    different success will return Success(...)                              ${ TryDifferent(PrimitiveDifference(1, 2), isSuccess = true).render must_=== "Success(1 != 2)" }
    different failure will return Failure(...)                              ${ TryDifferent(PrimitiveDifference(1, 2), isSuccess = false).render must_=== "Failure(1 != 2)" }
    different success failure type will be Success(...) ==> Failure(...)    ${ TryTypeDifferent(isActualSuccess = true).render must_=== "Success(...) ==> Failure(...)" }
    different failure success type will be Success(...) ==> Failure(...)    ${ TryTypeDifferent(isActualSuccess = false).render must_=== "Failure(...) ==> Success(...)" }


    Case Class Render:
    ==================

    Identical will show the class name SomeClass(...)                   $c1
    Different will show the class name and list of changes in order     $c2

    Other Render:
    =============

    OtherIdentical will return actual.toString                                                  $ot1
    OtherDifference will return actual.toString != expected.toString                            $ot2
    Unknown types where a primitive is compared to an object should print explicit class type   $ot3
    when comparing two different objects, don't add explicit class type                         $ot4

  """

  def p1 = { PrimitiveIdentical(5l).render must_=== "5" }
  def p2 = { PrimitiveIdentical("str").render must_=== "'str'" }

  def p3 = { PrimitiveDifference(5l, 6l).render must_=== "5 != 6" }
  def p4 = { PrimitiveDifference("str", "anotherStr").render must_=== s"'str' != 'anotherStr'" }
  def p5 = { PrimitiveIdentical(null).render must_=== "null" }

  def m1 = { MapIdentical(Map("x" -> "y")).render must_== s"Map('x' -> 'y')" }
  def m2 = { MapDifference(identical = Seq("x" -> "y"), Seq.empty, Seq.empty, Seq.empty).render must_== "Map('x' -> 'y')" }
  def m3 = { MapDifference(Seq.empty, changed = Seq("k" -> PrimitiveDifference("x", "y")), Seq.empty, Seq.empty).render must_== "Map('k' -> {'x' != 'y'})" }
  def m4 = { MapDifference(Seq.empty, Seq.empty, added = Seq("x" -> "y"), Seq.empty).render must_== "Map(added: 'x' -> 'y')" }
  def m5 = { MapDifference(Seq.empty, Seq.empty, Seq.empty, removed = Seq("x" -> "y")).render must_== "Map(removed: 'x' -> 'y')" }
  def m6 = {
    MapDifference(identical = Seq("a" -> "b"),
                  changed = Seq("c" -> PrimitiveDifference("d", "x")),
                  added = Seq("g" -> "h"),
                  removed = Seq("e" -> "f")).render must_==
      "Map('a' -> 'b', 'c' -> {'d' != 'x'}, added: 'g' -> 'h', removed: 'e' -> 'f')"
  }

  def o1 = { OptionIdentical(Some(PrimitiveIdentical("abc"))).render must_== "Some('abc')" }
  def o2 = { OptionIdentical(None).render must_== "None" }
  def o3 = { OptionDifferent(PrimitiveDifference("abc", "def")).render must_== "Some('abc' != 'def')" }
  def o4 = { OptionTypeDifferent(isActualSome = false, isExpectedSome = true).render must_== "None ==> Some(...)" }
  def o5 = { OptionTypeDifferent(isActualSome = true, isExpectedSome = false).render must_== "Some(...) ==> None" }

  def e1 = { EitherIdentical(PrimitiveIdentical("abc"), isRight = false).render must_=== "Left('abc')" }
  def e2 = { EitherIdentical(PrimitiveIdentical("abc"), isRight = true).render must_=== "Right('abc')" }
  def e3 = { EitherDifferent(PrimitiveDifference("abc", "def"), isRight = true).render must_=== "Right('abc' != 'def')" }
  def e4 = { EitherDifferent(PrimitiveDifference("abc", "def"), isRight = false).render must_=== "Left('abc' != 'def')" }
  def e5 = { EitherTypeDifferent(isActualRight = true).render must_=== "Right(...) ==> Left(...)" }
  def e6 = { EitherTypeDifferent(isActualRight = false).render must_=== "Left(...) ==> Right(...)" }


  def set1 = { SetIdentical(Set("a", "b")).render must_== "Set('a', 'b')" }
  def set2 = { SetDifference(identical = Seq("a", "b"), Seq.empty, Seq.empty).render must_== "Set('a', 'b')" }
  def set3 = { SetDifference(Seq.empty, added = Seq("c", "d"), Seq.empty).render must_== "Set(added: 'c', 'd')" }
  def set4 = { SetDifference(Seq.empty, Seq.empty, removed = Seq("e") ).render must_== "Set(removed: 'e')" }
  def set5 = { SetDifference(identical = Seq("a", "b"), added = Seq("c", "d"), removed = Seq("e")).render must_== "Set('a', 'b', added: 'c', 'd', removed: 'e')" }


  def seq1 = { SeqIdentical(Seq("a", "b")).render must_== "List('a', 'b')" }
  def seq2 = { SeqDifference(Seq(PrimitiveIdentical("a")), Seq.empty, Seq.empty).render must_== "List('a')" }
  def seq3 = { SeqDifference(Seq(PrimitiveDifference("b", "c")), Seq.empty, Seq.empty).render must_== "List('b' != 'c')" }
  def seq4 = { SeqDifference(Seq.empty, added = Seq("d", "e"), Seq.empty).render must_== "List(added: 'd', 'e')" }
  def seq5 = { SeqDifference(Seq.empty, Seq.empty, removed = Seq("f")).render must_== "List(removed: 'f')" }
  def seq6 = { SeqDifference(result = Seq(PrimitiveIdentical("a"), PrimitiveDifference("b", "c")),
                             added = Seq("d", "e"),
                             removed = Seq("f")).render must_== "List('a', 'b' != 'c', added: 'd', 'e', removed: 'f')" }

  def arr1 = { ArrayIdentical(Seq("a", "b")).render must_== "Array('a', 'b')" }
  def arr2 = { ArrayDifference(Seq(PrimitiveIdentical("a")), Seq.empty, Seq.empty).render must_== "Array('a')" }
  def arr3 = { ArrayDifference(Seq(PrimitiveDifference("b", "c")), Seq.empty, Seq.empty).render must_== "Array('b' != 'c')" }
  def arr4 = { ArrayDifference(Seq.empty, added = Seq("d", "e"), Seq.empty).render must_== "Array(added: 'd', 'e')" }
  def arr5 = { ArrayDifference(Seq.empty, Seq.empty, removed = Seq("f")).render must_== "Array(removed: 'f')" }
  def arr6 = { ArrayDifference(result = Seq(PrimitiveIdentical("a"), PrimitiveDifference("b", "c")),
                               added = Seq("d", "e"),
                               removed = Seq("f")).render must_== "Array('a', 'b' != 'c', added: 'd', 'e', removed: 'f')" }


  def c1 = { CaseClassIdentical("ClassName").render must_=== "ClassName(...)" }
  def c2 = { CaseClassDifferent("ClassName", Seq(CaseClassPropertyComparison("prop2", PrimitiveIdentical("val"), identical = true),
                                                 CaseClassPropertyComparison("prop", PrimitiveDifference("val1", "val2"), identical = false))).render must_=== "ClassName(prop2: 'val', prop: 'val1' != 'val2')" }

  def se1 = { StackElementIdentical(stackTraceElement).render must_=== stackTraceElement.toString }
  def se2 = { StackElementDifferent(PrimitiveDifference("class", "class1"),
                                    PrimitiveDifference("method", "method1"),
                                    Some(PrimitiveDifference("file", "file1")),
                                    PrimitiveDifference(666, 777)).render must_=== "'class' != 'class1'.'method' != 'method1'('file' != 'file1':666 != 777)" }
  def se3 = { StackElementDifferent(PrimitiveDifference("class", "class1"),
                                    PrimitiveDifference("method", "method1"),
                                    None,
                                    PrimitiveDifference(666, 777)).render must_=== "'class' != 'class1'.'method' != 'method1'(Unknown Source)" }

  def th1 = { ThrowableIdentical(ex).render must_=== { val w = new StringWriter(); ex.printStackTrace(new PrintWriter(w)); w.toString } }
  def th2 = { ThrowableDifferent(Seq(StackElementIdentical(stackTraceElement), StackElementIdentical(stackTraceElement)),
                                     Seq.empty, Seq.empty).render must_=== "'class.method(file:666)'\n\t'class.method(file:666)'" }

  def ot1 = { OtherIdentical(5).render must_=== "5" }
  def ot2 = { OtherDifferent(5, "5").render must_=== "5 != '5'" }
  def ot3 = { OtherDifferent(Hello(), "hello").render must_=== "hello: org.specs2.matcher.Hello != hello: java.lang.String" }
  def ot4 = { OtherDifferent(Set(1), Set.empty[Int]).render must_=== "Set(1) != Set()" }

  val stackTraceElement = new StackTraceElement("class", "method", "file", 666)
  val ex = new Exception
}
