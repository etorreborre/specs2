package org.specs2
package reflect

import scala.reflect.ClassTag
import control._
import org.specs2.control.eff.SafeEffect._
import org.portablescala.reflect.Reflect

trait Classes extends ClassOperations {

  type EnableReflectiveInstantiation =
    org.portablescala.reflect.annotation.EnableReflectiveInstantiation

  def newInstance(name: String, loader: ClassLoader): Any =
    Reflect
      .lookupInstantiatableClass(name, loader)
      .getOrElse(throw new ClassNotFoundException(name))
      .newInstance

  def loadModule(name: String, loader: ClassLoader): Any =
    Reflect
      .lookupLoadableModuleClass(name, loader)
      .getOrElse(throw new ClassNotFoundException(name))
      .loadModule

  def createInstance[T <: AnyRef](className: String)(implicit m: ClassTag[T]): Operation[T] =
    throw new Exception("Classes.createInstance: no js implementation")

  def createInstance[T <: AnyRef](className: String, loader: ClassLoader, defaultInstances: =>List[AnyRef] = Nil)(implicit m: ClassTag[T]): Operation[T] =
    if (className.endsWith("$"))
      protect[OperationStack, T](loadModule(className, loader).asInstanceOf[T])
    else
      protect[OperationStack, T](newInstance(className, loader).asInstanceOf[T])

  def createInstanceFromClass[T <: AnyRef](klass: Class[T], defaultInstances: =>List[AnyRef])(implicit m: ClassTag[T]): Operation[T] = {
    Use(m)
    throw new Exception("Classes.createInstanceFromClass: no js implementation")
  }

  def createInstanceFromClass[T <: AnyRef](klass: Class[T], loader: ClassLoader, defaultInstances: =>List[AnyRef] = Nil)(implicit m: ClassTag[T]): Operation[T] =
    createInstance(klass.getName, loader, defaultInstances)

  /** try to create an instance but return an exception if this is not possible */
  def createInstanceEither[T <: AnyRef](className: String, loader: ClassLoader, defaultInstances: =>List[AnyRef] = Nil)(implicit m: ClassTag[T]): Operation[Throwable Either T] =
    throw new Exception("Classes.createInstanceEither: no js implementation")

  def loadClassEither[T <: AnyRef](className: String, loader: ClassLoader): Operation[Throwable Either Class[T]] =
    throw new Exception("Classes.loadClassEither: no js implementation")

  def loadClass[T <: AnyRef](className: String, loader: ClassLoader): Operation[Class[T]] =
    throw new Exception("Classes.loadClass: no js implementation")

  def existsClass(className: String, loader: ClassLoader): Operation[Boolean] =
    throw new Exception("Classes.existsClass: no js implementation")

}

object Classes extends Classes
