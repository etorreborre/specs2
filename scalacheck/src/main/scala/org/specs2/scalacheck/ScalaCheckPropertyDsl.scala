package org.specs2
package scalacheck

import org.scalacheck.{Test, Prop}

trait ScalaCheckPropertyDsl {
  implicit def propToScalaCheckProperty(prop: Prop)(implicit parameters: Parameters): ScalaCheckProp =
    ScalaCheckProp(prop, parameters)
}

case class ScalaCheckProp(prop: Prop, parameters: Parameters) extends ScalaCheckProperty {
  type SelfType = ScalaCheckProp

  def setParameters(ps: Parameters): SelfType =
    copy(parameters = ps)
}
