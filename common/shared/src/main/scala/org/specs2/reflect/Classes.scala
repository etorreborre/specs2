package org.specs2
package reflect

import scala.reflect.ClassTag
import ClassName._
import control._
import scala.util.control.NonFatal
import org.specs2.fp.syntax._
import java.lang.reflect.Constructor
import eff._

/**
 * This trait provides functions to instantiate classes
 */
trait Classes {

  /**
   * Try to create an instance of a given class by using whatever constructor is available
   * and trying to instantiate the first parameter recursively if there is a parameter for that constructor.
   *
   * This is useful to instantiate nested classes which are referencing their outer class in their constructor
   */
  def createInstance[T <: AnyRef](className: String, loader: ClassLoader, defaultInstances: List[AnyRef] = Nil)(implicit m: ClassTag[T]): Operation[T] =
    loadClass(className, loader) >>= { klass: Class[T] =>
      createInstanceFromClass(klass, loader, defaultInstances)
    }

  def createInstanceFromClass[T <: AnyRef](klass: Class[T], loader: ClassLoader, defaultInstances: List[AnyRef] = Nil)(implicit m: ClassTag[T]): Operation[T] =
    findInstance[T](klass, loader, defaultInstances,
      klass.getDeclaredConstructors.toList.filter(_.getParameterTypes.size <= 1).sortBy(_.getParameterTypes.size))

  /** try to create an instance but return an exception if this is not possible */
  def createInstanceEither[T <: AnyRef](className: String, loader: ClassLoader, defaultInstances: List[AnyRef] = Nil)(implicit m: ClassTag[T]): Operation[Throwable Either T] =
    loadClassEither(className, loader) >>= { tc: Throwable Either Class[T] =>
      tc match {
        case Left(t) => Operations.ok(Left(t))
        case Right(klass) =>
          findInstance[T](klass, loader, defaultInstances,
            klass.getDeclaredConstructors.toList.filter(_.getParameterTypes.size <= 1).sortBy(_.getParameterTypes.size)).map(Right(_))
      }
    }

  private def findInstance[T <: AnyRef : ClassTag](klass: Class[T], loader: ClassLoader, defaultInstances: List[AnyRef], cs: List[Constructor[_]], error: Option[ErrorEffect.Error] = None): Operation[T] =
    cs match {
      case Nil => error.map(Operations.fromError[T]).getOrElse(Operations.fail[T]("Can't find a constructor for class "+klass.getName))
      case c :: rest =>
        runOperation(createInstanceForConstructor[T](klass, c, loader, defaultInstances)).
          fold(e => findInstance[T](klass, loader, defaultInstances, rest, Some(e)),
            a => Operations.delayed[T](a))
    }


  /**
   * Given a class, a zero or one-parameter constructor, return an instance of that class
   */
  private def createInstanceForConstructor[T <: AnyRef : ClassTag](klass: Class[_], constructor: Constructor[_],
                                                                   loader: ClassLoader, defaultInstances: List[AnyRef] = Nil): Operation[T] = {

    constructor.setAccessible(true)
    if (constructor.getParameterTypes.isEmpty)
      newInstance(klass, constructor.newInstance())

    else if (constructor.getParameterTypes.size == 1) {
      defaultInstances.find(i => constructor.getParameterTypes.apply(0) isAssignableFrom i.getClass) match {
        case None =>
          // if the specification has a constructor with one parameter, it is either because
          // it is a nested class
          // or it might have a parameter that has a 0 args constructor
          val constructorParameter =
            createInstance(constructor.getParameterTypes.toSeq(0).getName, loader, defaultInstances).
              orElse(createInstance[T](getOuterClassName(klass), loader, defaultInstances))

          constructorParameter.flatMap(p => newInstance(klass, constructor.newInstance(p)))

        case Some(instance) =>
          newInstance(klass, constructor.newInstance(instance))
      }
    } else Operations.fail[T]("Can't find a suitable constructor for class "+klass.getName)
  }

  /** create a new instance for a given class and return a proper error if this fails */
  private def newInstance[T](klass: Class[_], instance: =>Any): Operation[T] =
    try Operations.ok(instance.asInstanceOf[T])
    catch { case NonFatal(t) =>
      Operations.exception(UserException("cannot create an instance for class " + klass.getName, t))
    }

  /**
   * Load a class, given the class name
   */
  def loadClassEither[T <: AnyRef](className: String, loader: ClassLoader): Operation[Throwable Either Class[T]] = Operations.delayed {
    try Right(loader.loadClass(className).asInstanceOf[Class[T]])
    catch { case NonFatal(t) => Left(t) }
  }

  def loadClass[T <: AnyRef](className: String, loader: ClassLoader): Operation[Class[T]] =
    loadClassEither(className, loader).flatMap((tc: Throwable Either Class[T]) => tc.fold(Operations.exception, Operations.ok))

  /** @return true if a class can be loaded */
  def existsClass(className: String, loader: ClassLoader): Operation[Boolean] = Operations.delayed {
    try   { loader.loadClass(className); true }
    catch { case NonFatal(t) => false }
  }

}
/**
 * This object provides simple functions to instantiate classes.
 */
object Classes extends Classes
