package org.specs2
package reflect

import control.Exceptions._

/**
 * Utility reflection methods
 */
private[specs2]
trait ClassName {
  /**
   * @return the class name of an instance
   */
  def className(any: AnyRef): String = className(any.getClass.getName)
  /**
   * @return a description from the class name, taking the last name which doesn't contain a $ or a number.
   * For example: com.pack1.MyClass$1$ will:<ul>
   * <li>split on $ and reverse: [1, com.pack1.MyClass]
   * <li>drop the every element which is an integer -> [com.pack1.MyClass]
   * <li>take the first element: com.pack1.MyClass
   * <li>split on . and reverse: [MyClass, pack1, com]
   * <li>take the last element: MyClass</ul>
   */
  def className(className: String): String = {
   className.
    split("\\$").reverse.
    dropWhile(i => tryOk(i.toInt))(0).
    split("\\.").
    reverse.toList(0)
  }
  /**
   * @return the simple name of a class
   */
  def simpleName(klass: Class[_]) = {
    className(klass.getName).split("\\.").last
  }
}
private[specs2]
object ClassName extends ClassName