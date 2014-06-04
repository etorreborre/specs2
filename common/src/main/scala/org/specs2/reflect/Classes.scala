package org.specs2
package reflect

import control._
import ClassName._

import scala.reflect.ClassTag
import ClassName._
import control._
import Actions._
import scalaz.{-\/, \/}
import scalaz.std.anyVal._
import scalaz.syntax.std.option._
import java.lang.reflect.Constructor

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
  def createInstance[T <: AnyRef](className: String, loader: ClassLoader, parameter: Option[AnyRef] = None)(implicit m: ClassTag[T]): Action[T] =
    Actions.reader { configuration  =>
      loadClass(className, loader).map { klass: Class[T] =>
        val constructors = klass.getDeclaredConstructors.toList.filter(_.getParameterTypes.size <= 1).sortBy(_.getParameterTypes.size)

        if (constructors.isEmpty)
          Actions.fail("Can't find a constructor for class "+klass.getName)
        else {
          // how to get the first successful action?
          val results = constructors.map { constructor =>
            createInstanceForConstructor(klass, constructor, loader, parameter).execute(configuration).unsafePerformIO
          }
          val result: Action[T] = results.find(_.isOk).cata(Actions.fromStatus,
            results.map(Actions.fromStatus).headOption.getOrElse(Actions.fail("no cause")))
          result
        }
      }.flatMap(identity)
    }.flatMap(identity)

  /**
   * Try to create an instance of a given class by using whatever constructor is available
   * and return either the instance or an exception
   */
  def createInstanceEither[T <: AnyRef](className: String, loader: ClassLoader, parameter: Option[AnyRef] = None)(implicit m: ClassTag[T]): Action[Throwable \/ T] =
    Actions.reader { configuration  =>
      loadClass(className, loader).map { klass: Class[T] =>
        val constructors = klass.getDeclaredConstructors.toList.filter(_.getParameterTypes.size <= 1).sortBy(_.getParameterTypes.size)

        if (constructors.isEmpty)
          Actions.ok(-\/(new Exception("Can't find a constructor for class "+klass.getName)))
        else {
          // how to get the first successful action?
          val results = constructors.map { constructor =>
            createInstanceForConstructor(klass, constructor, loader, parameter).execute(configuration).unsafePerformIO
          }
          val result: Action[Throwable \/ T] =
            results
              .find(_.isOk)
              .cata(Actions.fromStatusAsDisjunction[T],
                    results.map(Actions.fromStatusAsDisjunction).headOption.getOrElse(Actions.ok[Throwable \/ T](-\/(new Exception("no cause")))))
          result
        }
      }.flatMap(identity)
    }.flatMap(identity)


  /**
   * Given a class, a zero or one-parameter constructor, return an instance of that class
   */
  private def createInstanceForConstructor[T <: AnyRef : ClassTag](c: Class[_], constructor: Constructor[_],
                                                                   loader: ClassLoader, parameter: Option[AnyRef] = None): Action[T] = Actions.safe {
    if (constructor.getParameterTypes.isEmpty)
      Actions.safe(constructor.newInstance().asInstanceOf[T])

    else if (constructor.getParameterTypes.size == 1) {
      // if the specification has a construction, it is either because it is a nested class
      // or if it has an Arguments parameter
      // or it might have a parameter that has a 0 args constructor
      val constructorParameter =
        createInstance[T](getOuterClassName(c), loader)
          .orElse(Actions.ok(parameter))
          .orElse(createInstance[AnyRef](constructor.getParameterTypes.toSeq(0).getName, loader))

      constructorParameter.map(constructor.newInstance(_).asInstanceOf[T])
    } else Actions.fail("Can't find a suitable constructor for class "+c.getName)
  }.flatMap(action => action)

  /**
   * @return an instance of a given class, checking that the created instance typechecks as expected
   */
  def createInstanceFor[T <: AnyRef](klass: Class[T])(implicit m: ClassTag[T]): Action[T] = Actions.safe {
    val constructor = klass.getDeclaredConstructors()(0)
    constructor.setAccessible(true)
    val instance: AnyRef = constructor.newInstance().asInstanceOf[AnyRef]
    if (!m.runtimeClass.isInstance(instance)) Actions.fail(instance + " is not an instance of " + m.runtimeClass.getName)
    else Actions.safe(instance.asInstanceOf[T])
  }.flatMap(identity)

  /**
   * Load a class, given the class name
   */
  def loadClass[T <: AnyRef](className: String, loader: ClassLoader): Action[Class[T]] = Actions.safe {
    loader.loadClass(className).asInstanceOf[Class[T]]
  }

}
/**
 * This object provides simple functions to instantiate classes.
 */
object Classes extends Classes
