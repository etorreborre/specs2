package org.specs2
package reflect

import scala.reflect.ClassTag
import control._
import fp.syntax._
import org.portablescala.reflect.Reflect

trait Classes extends ClassOperations {

  type EnableReflectiveInstantiation =
    org.portablescala.reflect.annotation.EnableReflectiveInstantiation

  def newInstance(name: String): Any =
    Reflect
      .lookupInstantiatableClass(name)
      .getOrElse(throw new ClassNotFoundException(name))
      .newInstance

  def loadModule(name: String): Any =
    Reflect
      .lookupLoadableModuleClass(name)
      .getOrElse(throw new ClassNotFoundException(name))
      .loadModule

  def createInstance[T <: AnyRef](className: String)(implicit m: ClassTag[T]): Operation[T] =
    if (className.endsWith("$"))
      Operation.delayed(loadModule(className).asInstanceOf[T])
    else
      Operation.delayed(newInstance(className).asInstanceOf[T])

  def createInstance[T <: AnyRef](className: String, loader: ClassLoader, defaultInstances: =>List[AnyRef] = Nil)(implicit m: ClassTag[T]): Operation[T] =
    createInstance(className)(m)

  def createInstanceFromClass[T <: AnyRef](klass: Class[T], defaultInstances: =>List[AnyRef])(implicit m: ClassTag[T]): Operation[T] =
    createInstance(klass.getName)(m)

  def createInstanceFromClass[T <: AnyRef](klass: Class[T], loader: ClassLoader, defaultInstances: =>List[AnyRef] = Nil)(implicit m: ClassTag[T]): Operation[T] =
    createInstance(klass.getName)(m)

  /** try to create an instance but return an exception if this is not possible */
  def createInstanceEither[T <: AnyRef](className: String, loader: ClassLoader, defaultInstances: =>List[AnyRef] = Nil)(implicit m: ClassTag[T]): Operation[Throwable Either T] =
    try createInstance(className)(m).map(Right(_))
    catch {
      case e: Throwable => Operation.pure(Left(e))
    }

  def loadClassEither[T <: AnyRef](className: String, loader: ClassLoader): Operation[Throwable Either Class[T]] =
    throw new Exception("Classes.loadClassEither: no js implementation")

  def loadClass[T <: AnyRef](className: String, loader: ClassLoader): Operation[Class[T]] =
    throw new Exception("Classes.loadClass: no js implementation")

  def existsClass(className: String, loader: ClassLoader): Operation[Boolean] =
    Operation.pure(
      Reflect.lookupInstantiatableClass(className).nonEmpty ||
      Reflect.lookupLoadableModuleClass(className).nonEmpty)


}

object Classes extends Classes
