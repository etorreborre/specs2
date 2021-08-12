package org.specs2
package reflect

import scala.reflect.NameTransformer
import control.Exceptions.*
import text.CamelCase.*
import annotation.*

/** Reflection methods for Class names
  */
trait ClassName:
  outer: ClassName =>

  /** @return the class name of an instance */
  def simpleClassName(any: AnyRef): String =
    simpleName(any.getClass)

  /** @return the class name of an instance */
  def className(any: AnyRef): String =
    className(any.getClass)

  /** @return
    *   the outer class name for a given class
    */
  def getOuterClassName(c: Class[?]): String =
    c.getDeclaredConstructors.head.getParameterTypes.head.getName

  /** @return
    *   the decoded class name, with its package
    */
  def className(name: String): String =
    val decoded = NameTransformer.decode(name)
    val remainingDollarNames = decoded.split("\\$")
    val result = if remainingDollarNames.size > 1 then
      val lastName = remainingDollarNames(remainingDollarNames.size - 1)
      if lastName.matches("\\d") || lastName == "class" then remainingDollarNames(remainingDollarNames.size - 2)
      else lastName
    else remainingDollarNames(0)
    result

  /** @return
    *   the package name from the decoded class name
    */
  def packageName(name: String): String = className(name).split("\\.").dropRight(1).mkString(".")

  /** @return
    *   the class name
    */
  def className(klass: Class[?]): String = className(klass.getName)

  /** @return
    *   the class name without the package name
    */
  def simpleName(klass: Class[?]): String =
    // klass.getSimpleName can throw an error in the REPL
    val result = catchAllOrElse {
      val name = className(klass.getSimpleName)

      // the simpleName is empty for an anonymous class when jdk >= 9
      // in that case we use the full mangled name
      if name.isEmpty then klass.getName
      else name

    }(klass.getName)
    if result.contains("anon") && klass.getSuperclass != null then simpleName(klass.getSuperclass)
    else result

  /** @return
    *   the uncamelcased name of the class (or its parent if it is an anonymous class)
    */
  def humanName(c: Class[?]): String =
    val name = simpleName(c)
    if name.contains("$") && c.getSuperclass != null then humanName(c.getSuperclass)
    else name.camelCaseToWords

  extension (klass: Class[?])

    def simpleName(using nothing: Int = 0): String =
      outer.simpleName(klass)

    def humanName(using nothing: Int = 0): String =
      outer.humanName(klass)

object ClassName extends ClassName
