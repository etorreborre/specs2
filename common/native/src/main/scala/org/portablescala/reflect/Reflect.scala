package org.portablescala.reflect

import scala.scalanative.reflect.{Reflect => ScalaNativeReflect}

object Reflect {
  def lookupLoadableModuleClass(fqcn: String): Option[LoadableModuleClass] =
    ScalaNativeReflect.lookupLoadableModuleClass(fqcn)

  def lookupInstantiatableClass(fqcn: String): Option[InstantiatableClass] =
    ScalaNativeReflect.lookupInstantiatableClass(fqcn)
}
