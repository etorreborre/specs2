package org.specs2
package specification
package core

import org.scalacheck.*, Gen.*, Arbitrary.*
import create.*
import execute.StandardResults.*
import FormattingFragments.*
import DefaultFragmentFactory.*

object Arbitraries:

  given FragmentArbitrary: Arbitrary[Fragment] =
    Arbitrary {
      Gen.oneOf(
        genExample,
        genText,
        genStep,
        genFormatting
      )
    }

  given FragmentsArbitrary: Arbitrary[Fragments] =
    Arbitrary {
      Gen.listOf(arbitrary[Fragment]).map(fs => Fragments(fs*))
    }

  def genExample: Gen[Fragment] =
    alphaStr.map(text => example(text, Execution.executed(success)))

  def genText: Gen[Fragment] =
    alphaStr.map(text)

  def genStep: Gen[Fragment] =
    Gen.const(step(success))

  def genFormatting: Gen[Fragment] =
    Gen.oneOf(
      Seq(br, t, t(2), bt, bt(2), DefaultFragmentFactory.end)
    )
