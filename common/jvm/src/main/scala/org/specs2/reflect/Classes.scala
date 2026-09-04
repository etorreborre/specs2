package org.specs2
package reflect

import scala.reflect.{ClassTag, NameTransformer}
import scala.util.control.NonFatal
import java.lang.reflect.Constructor
import control.*
import fp.syntax.*
import ClassName.*

/** This trait provides functions to instantiate classes
  */
trait Classes extends ClassOperations:

  /** Try to create an instance of a given class by using whatever constructor is available and trying to instantiate
    * the first parameter recursively if there is a parameter for that constructor.
    *
    * This is useful to instantiate nested classes which are referencing their outer class in their constructor
    */
  def createInstanceFromName[T <: AnyRef](className: String, defaultInstances: =>List[AnyRef] = Nil)(using
      m: ClassTag[T]
  ): Operation[T] =
    createInstance(className, getClass.getClassLoader)

  def createInstance[T <: AnyRef](className: String, loader: ClassLoader, defaultInstances: =>List[AnyRef] = Nil)(using
      m: ClassTag[T]
  ): Operation[T] =
    loadClass(className, loader) >>= { (klass: Class[T]) =>
      createInstanceFromClass(klass, loader, defaultInstances)
    }

  def createInstanceFromClass[T <: AnyRef](klass: Class[T], defaultInstances: =>List[AnyRef])(using
      m: ClassTag[T]
  ): Operation[T] =
    createInstanceFromClass(klass, klass.getClassLoader, defaultInstances)

  def createInstanceFromClass[T <: AnyRef](
      klass: Class[T],
      loader: ClassLoader,
      defaultInstances: =>List[AnyRef] = Nil
  )(using m: ClassTag[T]): Operation[T] =
    findInstance[T](
      klass,
      loader,
      defaultInstances,
      klass.getDeclaredConstructors.toList.filter(_.getParameterTypes.size <= 1).sortBy(_.getParameterTypes.size)
    )

  /** try to create an instance but return an exception if this is not possible */
  def createInstanceEither[T <: AnyRef](
      className: String,
      loader: ClassLoader,
      defaultInstances: =>List[AnyRef] = Nil
  )(using m: ClassTag[T]): Operation[Throwable Either T] =
    loadClassEither(className, loader) >>= { (tc: Throwable Either Class[T]) =>
      tc match
        case Left(t) =>
          Operation.ok(Left(t))
        case Right(klass) =>
          findInstance[T](
            klass,
            loader,
            defaultInstances,
            klass.getDeclaredConstructors.toList.filter(_.getParameterTypes.size <= 1).sortBy(_.getParameterTypes.size)
          ).map(Right(_))
    }

  private def findInstance[T <: AnyRef: ClassTag](
      klass: Class[T],
      loader: ClassLoader,
      defaultInstances: =>List[AnyRef],
      cs: List[Constructor[?]],
      error: Option[Throwable] = None
  ): Operation[T] =
    cs match
      case Nil =>
        error
          .map(Operation.exception[T])
          .getOrElse(
            Operation.fail[T]("Can't find a suitable constructor with 0 or 1 parameter for class " + klass.getName)
          )

      case c :: rest =>
        createInstanceForConstructor[T](klass, c, loader, defaultInstances).runOperation
          .fold(e => findInstance[T](klass, loader, defaultInstances, rest, Some(e)), a => Operation.delayed[T](a))

  /** Given a class, a zero or one-parameter constructor, return an instance of that class
    */
  private def createInstanceForConstructor[T <: AnyRef: ClassTag](
      klass: Class[?],
      constructor: Constructor[?],
      loader: ClassLoader,
      defaultInstances: =>List[AnyRef]
  ): Operation[T] = {

    constructor.setAccessible(true)
    if constructor.getParameterTypes.isEmpty then {
      if klass.getName.endsWith("$") then {
        newInstance(klass, klass.getDeclaredField(NameTransformer.MODULE_INSTANCE_NAME).get(null))
      } else {
        newInstance(klass, constructor.newInstance())
      }
    } else if constructor.getParameterTypes.size == 1 then {
      defaultInstances.find(i => constructor.getParameterTypes.apply(0).isAssignableFrom(i.getClass)) match {
        case None =>
          // if the specification has a constructor with one parameter, it is either because
          // it is a nested class
          // or it might have a parameter that has a 0 args constructor
          val constructorParameter =
            createInstance(constructor.getParameterTypes.toSeq(0).getName, loader, defaultInstances)
              .orElse(createInstance[T](getOuterClassName(klass), loader, defaultInstances))

          constructorParameter.flatMap(p => newInstance(klass, constructor.newInstance(p)))

        case Some(instance) =>
          newInstance(klass, constructor.newInstance(instance))
      }
    } else Operation.fail[T]("Can't find a suitable constructor for class " + klass.getName)
  }

  /** create a new instance for a given class and return a proper error if this fails */
  private def newInstance[T](klass: Class[?], instance: =>Any): Operation[T] =
    try Operation.ok(instance.asInstanceOf[T])
    catch {
      case NonFatal(t) =>
        Operation.exception(UserException("cannot create an instance for class " + klass.getName, t))
    }

  /** Load a class, given the class name
    */
  def loadClassEither[T <: AnyRef](className: String, loader: ClassLoader): Operation[Throwable Either Class[T]] =
    Operation.delayed {
      try Right(loader.loadClass(className).asInstanceOf[Class[T]])
      catch { case NonFatal(t) => Left(t) }
    }

  def loadClass[T <: AnyRef](className: String, loader: ClassLoader): Operation[Class[T]] =
    loadClassEither(className, loader).flatMap((tc: Throwable Either Class[T]) =>
      tc.fold(Operation.exception, Operation.ok)
    )

  /** @return true if a class can be loaded */
  def existsClass(className: String, loader: ClassLoader): Operation[Boolean] = Operation.delayed {
    try { loader.loadClass(className); true }
    catch { case NonFatal(t) => false }
  }

/** This object provides simple functions to instantiate classes.
  */
object Classes extends Classes
