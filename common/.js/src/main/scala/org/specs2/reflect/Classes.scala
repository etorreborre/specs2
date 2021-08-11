package org.specs2
package reflect

import control.*
import scala.reflect.ClassTag
import org.portablescala.reflect.*

trait Classes extends ClassOperations:

  type EnableReflectiveInstantiation =
    org.portablescala.reflect.annotation.EnableReflectiveInstantiation

  def newInstance(name: String, defaultInstances: =>List[AnyRef] = Nil): Any =
    newInstance(Reflect.lookupInstantiatableClass(name)
      .getOrElse(throw new ClassNotFoundException(name)), defaultInstances)

  def newInstance(klass: InstantiatableClass, defaultInstances: =>List[AnyRef]): Any = {
    defaultInstances match {
      case Nil => klass.newInstance
      case (h :: t) =>
        klass.getConstructor(h.getClass) match {
          case Some(c) => c.newInstance(h)
          case None => newInstance(klass, t)
        }
    }
  }

  def loadModule(name: String): Any =
    Reflect
      .lookupLoadableModuleClass(name)
      .getOrElse(throw new ClassNotFoundException(name))
      .loadModule

  def createInstanceFromName[T <: AnyRef](className: String, defaultInstances: =>List[AnyRef] = Nil)(using m: ClassTag[T]): Operation[T] =
    if (className.endsWith("$"))
      Operation.delayed(loadModule(className).asInstanceOf[T])
    else
      Operation.delayed(newInstance(className, defaultInstances).asInstanceOf[T])

  def createInstance[T <: AnyRef](className: String, loader: ClassLoader, defaultInstances: =>List[AnyRef] = Nil)(using m: ClassTag[T]): Operation[T] =
    createInstanceFromName[T](className, defaultInstances)

  def createInstanceFromClass[T <: AnyRef](klass: Class[T], defaultInstances: =>List[AnyRef])(using m: ClassTag[T]): Operation[T] =
    createInstanceFromName[T](klass.getName, defaultInstances)

  def createInstanceFromClass[T <: AnyRef](klass: Class[T], loader: ClassLoader, defaultInstances: =>List[AnyRef] = Nil)(using m: ClassTag[T]): Operation[T] =
    createInstance(klass.getName, loader, defaultInstances)

  /** try to create an instance but return an exception if this is not possible */
  def createInstanceEither[T <: AnyRef](className: String, loader: ClassLoader, defaultInstances: =>List[AnyRef] = Nil)(using m: ClassTag[T]): Operation[Throwable Either T] =
    try {
      createInstanceFromName[T](className, defaultInstances).map(Right(_))
    } catch {
      case e: Throwable => Operation.delayed(Left(e))
    }

  def loadClassEither[T <: AnyRef](className: String, loader: ClassLoader): Operation[Throwable Either Class[T]] =
    throw new Exception("Classes.loadClassEither: no js implementation")

  def loadClass[T <: AnyRef](className: String, loader: ClassLoader): Operation[Class[T]] =
    throw new Exception("Classes.loadClass: no js implementation")

  def existsClass(className: String, loader: ClassLoader): Operation[Boolean] =
    Operation.delayed(
      Reflect.lookupInstantiatableClass(className).nonEmpty ||
      Reflect.lookupLoadableModuleClass(className).nonEmpty)


object Classes extends Classes
