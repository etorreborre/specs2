package org.specs2.matcher.describe

import org.specs2.reflect.ClassName
import shapeless.labelled._
import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Witness}

/**
 * Diffable instance for a case class
 */
class CaseClassDiffable[T <: Product with Serializable with AnyRef: IsCaseClass, L <: HList](
  implicit labelled: LabelledGeneric.Aux[T, L], di: CaseClassIntrospection[L]) extends Diffable[T] {

  def diff(actual: T, expected: T): ComparisonResult =
    di.diff(labelled.to(actual), labelled.to(expected)).toComparisonResult(ClassName.simpleClassName(actual))
}

/**
 * Compare 2 case class instances recursively
 */
trait CaseClassIntrospection[-T] {
  def diff(actual: T, expected: T): CaseClassComparisonResult
}

object CaseClassIntrospection {

  implicit val hNilInspector: CaseClassIntrospection[HNil] =
    new HNilDifferenceInspectable

  implicit def caseClassFieldsInspector[Key <: Symbol, Value, Tail <: HList](
    implicit key:      Witness.Aux[Key],
             di:       Diffable[Value],
             diffTail: Lazy[CaseClassIntrospection[Tail]]) =

    new ClassFieldsDifferenceInspectable[Key, Value, Tail]

}

case class CaseClassComparisonResult(result: Seq[CaseClassPropertyComparison]) {
  def prepend(r: CaseClassPropertyComparison): CaseClassComparisonResult =
    copy(result = r +: result)

  def identical: Boolean =
    result.forall(_.identical)

  def toComparisonResult(className: String): ComparisonResult =
    if (identical) CaseClassIdentical(className)
    else           CaseClassDifferent(className, result)
}

object CaseClassComparisonResult {
  val empty = CaseClassComparisonResult(Seq.empty)
}


class HNilDifferenceInspectable extends CaseClassIntrospection[HNil] {
  def diff(actual: HNil, expected: HNil) = CaseClassComparisonResult.empty
}

class ClassFieldsDifferenceInspectable[Key <: Symbol, Value, Tail <: HList](
  implicit key:      Witness.Aux[Key],
           di:       Diffable[Value],
           diffTail: Lazy[CaseClassIntrospection[Tail]]) extends CaseClassIntrospection[FieldType[Key, Value] :: Tail] {

  def diff(actual: FieldType[Key, Value] :: Tail, expected: FieldType[Key, Value] :: Tail) =
    diffTail.value
            .diff(actual.tail, expected.tail)
            .prepend( compareHead(key.value.name, actual.head, expected.head) )

  private def compareHead(fieldName: String, actual: Value, expected: Value) =
    CaseClassPropertyComparison(fieldName = fieldName,
                            result = di.diff(actual, expected),
                            identical = actual == expected)
}

