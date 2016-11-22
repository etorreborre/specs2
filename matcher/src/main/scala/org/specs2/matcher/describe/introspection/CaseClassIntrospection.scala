package org.specs2.matcher.describe.introspection

import org.specs2.matcher.describe.constraints.IsCaseClass
import org.specs2.matcher.describe._
import shapeless.labelled._
import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Witness}

trait CaseClassIntrospection[-T]{
  def diff(actual: T, expected: T): CaseClassComparison
}

object CaseClassIntrospection {
  implicit val hNilInspector: CaseClassIntrospection[HNil] = new HNilDifferenceInspectable
  implicit def caseClassFieldsInspector[Key <: Symbol, Value, Tail <: HList](implicit key:      Witness.Aux[Key],
                                                                                      di:       Diffable[Value],
                                                                                      diffTail: Lazy[CaseClassIntrospection[Tail]]) =
    new ClassFieldsDifferenceInspectable[Key, Value, Tail]
}

case class CaseClassComparison(result: Seq[CaseClassPropertyComparison]) {
  def prepend(r: CaseClassPropertyComparison) = copy(result = r +: result)
  def identical = result.forall( _.identical )
}

object CaseClassComparison {
  val empty = CaseClassComparison(Seq.empty)
}


class HNilDifferenceInspectable extends CaseClassIntrospection[HNil] {
  def diff(actual: HNil, expected: HNil) = CaseClassComparison.empty
}

class ClassFieldsDifferenceInspectable[Key <: Symbol, Value, Tail <: HList](implicit
                                                                            key:      Witness.Aux[Key],
                                                                            di: Diffable[Value],
                                                                            diffTail: Lazy[CaseClassIntrospection[Tail]])
  extends CaseClassIntrospection[FieldType[Key, Value] :: Tail] {

  def diff(actual: FieldType[Key, Value] :: Tail, expected: FieldType[Key, Value] :: Tail) =
    diffTail.value
            .diff(actual.tail, expected.tail)
            .prepend( compareHead(key.value.name, actual.head, expected.head) )

  private def compareHead(fieldName: String, actual: Value, expected: Value) =
    CaseClassPropertyComparison(fieldName = fieldName,
                            result = di.diff(actual, expected),
                            identical = actual == expected)
}

class CaseClassDiffable[T <: Product with Serializable: IsCaseClass,
                                     L <: HList]
  (implicit labelled: LabelledGeneric.Aux[T, L],
            di: CaseClassIntrospection[L])
  extends Diffable[T] {

  def diff(actual: T, expected: T) =
    di.diff(labelled.to(actual), labelled.to(expected)) match {
      case r: CaseClassComparison if r.identical => CaseClassIdentical(actual.getClass.getSimpleName)
      case r                                     => CaseClassDifferent(actual.getClass.getSimpleName, r.result)
    }
}
