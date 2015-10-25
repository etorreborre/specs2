package org.specs2
package scalacheck

import org.scalacheck.util._
import org.scalacheck.{Prop}

trait ScalaCheckPropertyDsl {
  implicit def propToScalaCheckProperty(prop: Prop)(implicit parameters: Parameters, prettyFreqMap: FreqMap[Set[Any]] => Pretty): ScalaCheckProp =
    ScalaCheckProp(prop, parameters, prettyFreqMap)
}

case class ScalaCheckProp(prop: Prop, parameters: Parameters, prettyFreqMap: FreqMap[Set[Any]] => Pretty) extends ScalaCheckProperty {
  type SelfType = ScalaCheckProp

  def setParameters(ps: Parameters): SelfType =
    copy(parameters = ps)

  def setPrettyFreqMap(f: FreqMap[Set[Any]] => Pretty): SelfType =
    copy(prettyFreqMap = f)
}
