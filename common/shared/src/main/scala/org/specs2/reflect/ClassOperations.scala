package org.specs2
package reflect

import scala.reflect.ClassTag
import control._

/**
 * This trait provides functions to instantiate classes
 */
trait ClassOperations {

  /**
   * Try to create an instance of a given class by using whatever constructor is available
   * and trying to instantiate the first parameter recursively if there is a parameter for that constructor.
   *
   * This is useful to instantiate nested classes which are referencing their outer class in their constructor
   */
  def createInstance[T <: AnyRef](className: String)(implicit m: ClassTag[T]): Operation[T]

  def createInstance[T <: AnyRef](className: String, loader: ClassLoader, defaultInstances: =>List[AnyRef] = Nil)(implicit m: ClassTag[T]): Operation[T]

  def createInstanceFromClass[T <: AnyRef](klass: Class[T], loader: ClassLoader, defaultInstances: =>List[AnyRef] = Nil)(implicit m: ClassTag[T]): Operation[T]

  /** try to create an instance but return an exception if this is not possible */
  def createInstanceEither[T <: AnyRef](className: String, loader: ClassLoader, defaultInstances: =>List[AnyRef] = Nil)(implicit m: ClassTag[T]): Operation[Throwable Either T]

  /**
   * Load a class, given the class name
   */
  def loadClassEither[T <: AnyRef](className: String, loader: ClassLoader): Operation[Throwable Either Class[T]]

  def loadClass[T <: AnyRef](className: String, loader: ClassLoader): Operation[Class[T]]

  /** @return true if a class can be loaded */
  def existsClass(className: String, loader: ClassLoader): Operation[Boolean]

}
