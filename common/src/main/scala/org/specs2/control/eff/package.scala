package org.specs2.control

import scalaz._

package object eff {

  type <=[M[_], R] = Member.<=[M, R]

  object eff extends EffCreation with EffInterpretation with Effects
  object reader extends ReaderCreation with ReaderInterpretation
  object writer extends WriterCreation with WriterInterpretation
  object state extends StateCreation with StateInterpretation
  object eval extends EvalCreation with EvalInterpretation
  object option extends OptionCreation with OptionInterpretation
  object list extends ListCreation with ListInterpretation
  object disjunction extends DisjunctionCreation with DisjunctionInterpretation
  object console extends ConsoleEffect with ConsoleImplicits
  object warnings extends WarningsEffect with WarningsImplicits

  object all extends
    ConsoleEffect with
    WarningsEffect with
    ReaderEffect with
    WriterEffect with
    StateEffect with
    EvalEffect with
    OptionEffect with
    ListEffect with
    DisjunctionEffect with
    EffInterpretation with
    EffCreation with
    EffImplicits with
    Effects

  object interpret extends
    Interpret

  type Console[A] = Writer[String, A] @@ ConsoleTag
  type Warnings[A] = Writer[String, A] @@ WarningsTag

}
