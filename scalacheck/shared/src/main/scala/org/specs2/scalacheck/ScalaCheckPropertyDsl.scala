package org.specs2
package scalacheck

import org.scalacheck.util._
import org.scalacheck.{Properties, Prop}
import org.specs2.specification.core.Fragments
import org.specs2.specification.create.FragmentsFactory

trait ScalaCheckPropertyDsl extends FragmentsFactory with AsResultProp {
  implicit def propToScalaCheckProperty(prop: Prop)(implicit parameters: Parameters, prettyFreqMap: FreqMap[Set[Any]] => Pretty): ScalaCheckProp =
    ScalaCheckProp(prop, parameters, prettyFreqMap)

  /** display properties as examples */
  def properties(ps: Properties): Fragments =
    Fragments.foreach(ps.properties) { case (name, prop) =>
      Fragments(fragmentFactory.break, fragmentFactory.example(name, prop))
    }

}

case class ScalaCheckProp(prop: Prop, parameters: Parameters, prettyFreqMap: FreqMap[Set[Any]] => Pretty) extends ScalaCheckProperty {
  type SelfType = ScalaCheckProp

  def setParameters(ps: Parameters): SelfType =
    copy(parameters = ps)

  def setPrettyFreqMap(f: FreqMap[Set[Any]] => Pretty): SelfType =
    copy(prettyFreqMap = f)
}
