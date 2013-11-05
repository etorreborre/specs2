package org.specs2
package specification

import org.scalacheck._
import Arbitrary._
import main.Arguments
import control.LazyParameters._

trait ArbitraryFragments extends execute.StandardResults with FormattingFragments {

  implicit def arbitraryFragment: Arbitrary[Fragment] = Arbitrary {
    Gen.frequency ( 
      (1, Gen.const(Step(success))),
      (10, Gen.const(Text("text"))),
      (8, Gen.const(Example("ex1", success))),
      (1, Gen.const(end)),
      (2, Gen.const(bt)),
      (1, Gen.const(br)))
  }
  implicit def arbitraryFragments: Arbitrary[Fragments] = Arbitrary {

    def genFragments(sz: Int): Gen[Fragments] = for (l <- Gen.listOfN(sz, arbitrary[Fragment])) yield new Fragments(middle = l)
    def sizedList(sz: Int): Gen[Fragments] = {
      if (sz <= 0) genFragments(1)
      else genFragments(sz)
    }
    Gen.sized(sz => sizedList(sz))
  }
}