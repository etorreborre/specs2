package org.specs2.control

package object eff {

  type |=[M[_], R] = MemberIn.|=[M, R]
  type /=[M[_], R] = MemberInOut./=[M, R]
  type <=[M[_], R] = Member.<=[M, R]

  object eff         extends EffCreation          with EffInterpretation
  object writer      extends WriterCreation       with WriterInterpretation
  object eval        extends EvalEffect
  object state       extends StateCreation        with StateInterpretation
  object disjunction extends DisjunctionCreation  with DisjunctionInterpretation
  object safe        extends SafeCreation         with SafeInterpretation
  object async       extends AsyncEffect          with AsyncInterpretation

  object create extends
    WriterCreation with
    EvalCreation with
    StateCreation with
    DisjunctionCreation with
    SafeCreation with
    EffCreation

  object all extends
    WriterEffect with
    EvalEffect with
    StateEffect with
    DisjunctionEffect with
    SafeEffect with
    AsyncEffect with
    EffInterpretation with
    EffCreation with
    EffImplicits

  object interpret extends
    Interpret

}
