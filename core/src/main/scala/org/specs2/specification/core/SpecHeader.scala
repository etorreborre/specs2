package org.specs2
package specification
package core

import reflect.ClassName

case class SpecHeader(specClass: Class[_], title: Option[String] = None) {
  def show = title.getOrElse(simpleName)
  def className = ClassName.className(specClass)
  def simpleName = ClassName.simpleName(specClass)
}
