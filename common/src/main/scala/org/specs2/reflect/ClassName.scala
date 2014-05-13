package org.specs2
package reflect

import scala.reflect.NameTransformer
import control.Exceptions._
import text.CamelCase._

/**
 * Utility reflection methods for Class names
 */
private[specs2]
trait ClassName { outer =>
  /** @return the class name of an instance */
  def simpleClassName(any: AnyRef): String = simpleName(any.getClass)
  /** @return the class name of an instance */
  def className(any: AnyRef): String = className(any.getClass)
  /**
   * @return the outer class name for a given class
   */
  def getOuterClassName(c: Class[_]): String = {
    c.getDeclaredConstructors.toList(0).getParameterTypes.toList(0).getName
  }
  /**
   * @return the decoded class name, with its package
   */
  def className(name: String): String = {
    val decoded = NameTransformer.decode(name)
    val remainingDollarNames = decoded.split("\\$")
    val result = if (remainingDollarNames.size > 1) {
      val lastName = remainingDollarNames(remainingDollarNames.size - 1)
      if (lastName.matches("\\d") || lastName == "class")
        remainingDollarNames(remainingDollarNames.size - 2)
      else
        lastName
    } else remainingDollarNames(0)
    result
  }

  /**
   * @return the package name from the decoded class name
   */
  def packageName(name: String): String = className(name).split("\\.").dropRight(1).mkString(".")

  /**
   * @return the class name
   */
  def className(klass: Class[_]): String = className(klass.getName)
  /**
   * @return the class name without the package name
   */
  def simpleName(klass: Class[_]): String = {
    // klass.getSimpleName can throw an error in the REPL
    val result = catchAllOrElse(className(klass.getSimpleName))(klass.getName)
    if (result.contains("anon") && klass.getSuperclass != null) simpleName(klass.getSuperclass)
    else result
  }
  /**
   * @return the uncamelcased name of the class (or its parent if it is an anonymous class)
   */
  def humanName(c: Class[_]): String =
    if (c.simpleName.contains("$")) {
      if (c.getSuperclass != null) humanName(c.getSuperclass)
      else                         c.simpleName
    }
    else c.simpleName.camelCaseToWords

  implicit class ClassOps(klass: Class[_]) {
    def simpleName = outer.simpleName(klass)
    def humanName  = outer.humanName(klass)
  }
}
private[specs2]
object ClassName extends ClassName