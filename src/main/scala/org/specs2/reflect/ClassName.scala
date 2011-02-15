package org.specs2
package reflect

import scala.reflect.NameTransformer
import control.Exceptions._
import text.CamelCase._

/**
 * Utility reflection methods for Class names
 */
private[specs2]
trait ClassName {
  /** @return the class name of an instance */
  def className(any: AnyRef): String = className(any.getClass.getName)
  /** @return the simple name of a class */
  def simpleName(klass: Class[_]) = {
    className(klass.getName).split("\\.").last
  }
  /**
   * @return the outer class name for a given class
   */
  def getOuterClassName(c: Class[_]): String = {
    c.getDeclaredConstructors.toList(0).getParameterTypes.toList(0).getName
  }
  /**
   * @return the decoded class name
   */
  def className(name: String): String = {
    val decoded = NameTransformer.decode(name)
    val remainingDollarNames = decoded.split("\\$")
    val result = if (remainingDollarNames.size > 1) {
      if (remainingDollarNames(remainingDollarNames.size - 1).matches("\\d"))
        remainingDollarNames(remainingDollarNames.size - 2)
      else
        remainingDollarNames(remainingDollarNames.size - 1)
    } else remainingDollarNames(0)
    result
  }
  /**
   * @return the class name without the package name
   */
  def className(klass: Class[_]): String = {
    val result = className(klass.getSimpleName)
    if (result.contains("anon") && klass.getSuperclass != null)
      className(klass.getSuperclass)
    else
      result
  }
  /**
   * @return the class name without the package name of any object
   */
  def getClassName[T](a: T): String = className(a.asInstanceOf[java.lang.Object].getClass)
  /**
   * @return the uncamelcased name of the class (or its parent if it is an anonymous class)
   */
  def humanName(c: Class[_]): String =
    if (c.getSimpleName.contains("$"))
      humanName(c.getSuperclass)
    else
      c.getSimpleName.camelCaseToWords
}
private[specs2]
object ClassName extends ClassName