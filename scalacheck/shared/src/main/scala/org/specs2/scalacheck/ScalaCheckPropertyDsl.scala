package org.specs2
package scalacheck

import org.scalacheck.rng.Seed
import org.scalacheck.util._
import org.scalacheck.{Prop, Properties}
import org.specs2.specification.core.Fragments
import org.specs2.specification.create.FragmentsFactory

trait ScalaCheckPropertyDsl extends FragmentsFactory with AsResultProp:
  given propToScalaCheckProperty(using parameters: Parameters, prettyFreqMap: FreqMap[Set[Any]] => Pretty) as Conversion[Prop, ScalaCheckProp]:
    def apply(prop: Prop): ScalaCheckProp =
      ScalaCheckProp(prop, parameters, prettyFreqMap)

  /** display properties as examples */
  def properties(ps: Properties): Fragments =
    Fragments.foreach(ps.properties.toList) { case (name, prop) =>
      Fragments(fragmentFactory.break, fragmentFactory.example(name, prop))
    }


case class ScalaCheckProp(prop: Prop, parameters: Parameters, prettyFreqMap: FreqMap[Set[Any]] => Pretty) extends ScalaCheckProperty:
  type SelfType = ScalaCheckProp

  def setParameters(ps: Parameters): SelfType =
    copy(parameters = ps)

  def setSeed(seed: Seed): SelfType =
    copy(parameters = parameters.copy(seed = Some(seed)))

  def setSeed(seed: String): SelfType =
    copy(parameters = parameters.copy(seed = Parameters.makeSeed(seed)))

  def setPrettyFreqMap(f: FreqMap[Set[Any]] => Pretty): SelfType =
    copy(prettyFreqMap = f)
