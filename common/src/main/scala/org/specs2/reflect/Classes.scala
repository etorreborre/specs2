package org.specs2
package reflect

import scala.reflect.ClassTag
import ClassName._
import control._
import scala.util.control.NonFatal
import scalaz._, Scalaz._
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
  def createInstance[T <: AnyRef](className: String, loader: ClassLoader, defaultInstances: List[AnyRef] = Nil)(implicit m: ClassTag[T]): Action[T] =
    loadClass(className, loader) >>= { klass: Class[T] =>
      createInstanceFromClass(klass, loader, defaultInstances)
    }

  def createInstanceFromClass[T <: AnyRef](klass: Class[T], loader: ClassLoader, defaultInstances: List[AnyRef] = Nil)(implicit m: ClassTag[T]): Action[T] =
    findInstance[T](klass, loader, defaultInstances,
      klass.getDeclaredConstructors.toList.filter(_.getParameterTypes.size <= 1).sortBy(_.getParameterTypes.size))

  /** try to create an instance but return an exception if this is not possible */
  def createInstanceEither[T <: AnyRef](className: String, loader: ClassLoader, defaultInstances: List[AnyRef] = Nil)(implicit m: ClassTag[T]): Action[Throwable \/ T] =
    loadClassEither(className, loader) >>= { tc: Throwable \/ Class[T] =>
      tc match {
        case -\/(t) => Actions.ok(-\/(t))
        case \/-(klass) =>
          findInstance[T](klass, loader, defaultInstances,
            klass.getDeclaredConstructors.toList.filter(_.getParameterTypes.size <= 1).sortBy(_.getParameterTypes.size)).map(\/-(_))
      }
    }

  private def findInstance[T <: AnyRef : ClassTag](klass: Class[T], loader: ClassLoader, defaultInstances: List[AnyRef], cs: List[Constructor[_]], error: Option[ErrorEffect.Error] = None): Action[T] =
    cs match {
      case Nil => error.map(Actions.fromError[T]).getOrElse(Actions.fail[T]("Can't find a constructor for class "+klass.getName))
      case c :: rest =>
       runAction(createInstanceForConstructor[T](klass, c, loader, defaultInstances)).
         fold(e => findInstance[T](klass, loader, defaultInstances, rest, Some(e)),
              a => Actions.safe[T](a))
    }


  /**
   * Given a class, a zero or one-parameter constructor, return an instance of that class
   */
  private def createInstanceForConstructor[T <: AnyRef : ClassTag](c: Class[_], constructor: Constructor[_],
                                                                   loader: ClassLoader, defaultInstances: List[AnyRef] = Nil): Action[T] = {
    constructor.setAccessible(true)
    if (constructor.getParameterTypes.isEmpty)
      newInstance(c)(constructor.newInstance().asInstanceOf[T])

    else if (constructor.getParameterTypes.size == 1) {
      defaultInstances.find(i => constructor.getParameterTypes.apply(0) isAssignableFrom i.getClass) match {
        case None =>
          // if the specification has a constructor with one parameter, it is either because
          // it is a nested class
          // or it might have a parameter that has a 0 args constructor
          val constructorParameter =
            createInstance(constructor.getParameterTypes.toSeq(0).getName, loader, defaultInstances).
              orElse(createInstance[T](getOuterClassName(c), loader, defaultInstances))

          constructorParameter.flatMap(p => newInstance(c)(constructor.newInstance(p).asInstanceOf[T]))

        case Some(instance) =>
          newInstance(c)(constructor.newInstance(instance).asInstanceOf[T])
      }
    } else Actions.fail[T]("Can't find a suitable constructor for class "+c.getName)
  }

  /**
   * @return an instance of a given class, checking that the created instance typechecks as expected
   */
  def createInstanceFor[T <: AnyRef](klass: Class[T])(implicit m: ClassTag[T]): Action[T] = {
    val constructor = klass.getDeclaredConstructors()(0)
    constructor.setAccessible(true)
    newInstance(klass)(constructor.newInstance().asInstanceOf[AnyRef]).flatMap { instance =>
      if (!m.runtimeClass.isInstance(instance)) Actions.fail[T](instance + " is not an instance of " + m.runtimeClass.getName)
      else Actions.safe(instance.asInstanceOf[T])
    }
  }

  /** create a new instance for a given class and return a proper error if this fails */
  private def newInstance[T](klass: Class[_])(creation: =>T): Action[T] =
    try Actions.ok(creation)
    catch { case NonFatal(t) =>
      Actions.exception(UserException("cannot create an instance for class " + klass.getName, t))
    }

  /**
   * Load a class, given the class name
   */
  def loadClassEither[T <: AnyRef](className: String, loader: ClassLoader): Action[Throwable \/ Class[T]] = Actions.safe {
    try \/-(loader.loadClass(className).asInstanceOf[Class[T]])
    catch { case NonFatal(t) => -\/(t) }
  }

  def loadClass[T <: AnyRef](className: String, loader: ClassLoader): Action[Class[T]] =
    loadClassEither(className, loader).flatMap((tc: Throwable \/ Class[T]) => tc.fold(Actions.exception, Actions.ok))

}
/**
 * This object provides simple functions to instantiate classes.
 */
object Classes extends Classes
