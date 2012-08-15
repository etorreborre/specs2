package org.specs2
package reflect

import scala.reflect.Manifest
import ClassName._
import control.Exceptions._
import control.Throwablex._
import io._
import sys._
/**
 * This trait provides utility functions for classes
 */
private[specs2]
trait Classes extends Output {

  /** @return an instance of a given class, returning either the instance, or an exception */
  def create[T <: AnyRef](className: String = "", classLoader: ClassLoader = Thread.currentThread.getContextClassLoader)
                         (implicit m: Manifest[T]): Either[Throwable, T] =
    trye(createInstanceFor(loadClassOf[T](className, loader = classLoader)))

  /** @return an instance of a given class */
  def createObject[T <: AnyRef](className: String)(implicit m: Manifest[T]): Option[T] =
    createObject[T](className, false)(m)
  
  /** @return an instance of a given class and optionally print message if the class can't be loaded */
  def createObject[T <: AnyRef](className: String, printMessage: Boolean)
                               (implicit m: Manifest[T]): Option[T] =
    createObject(className, printMessage, false)(m)

  /**
   * A system property 'debugCreateObject' can be set to override the printMessage and printStackTrace parameters
   * so that the exception message and stacktrace are printed when the object can't be created
   * 
   * @return an instance of a given class and optionally print message and/or the stacktrace if the class can't be loaded.
   */
  def createObject[T <: AnyRef](className: String, printMessage: Boolean, printStackTrace: Boolean)
                               (implicit m: Manifest[T]): Option[T] = {
    tryo(createInstanceOf[T](loadClass[T](className, m.erasure.getClassLoader))) { (e: Exception) =>
      val debugCreateObject = sys.props("debugCreateObject") != null
      val shouldPrintStackTrace = printStackTrace || debugCreateObject
      val shouldPrintMessage = printMessage || debugCreateObject
      val msg = (shouldPrintMessage, shouldPrintStackTrace) match {
        case (_, true) => "Could not instantiate class: " + e.getFullStackTraceAsString
        case (true, false) => "Could not instantiate class: " + className + ": " + e.getMessage
        case (false, false) => ""
      }
      println(msg)
    }.flatMap(identity)
  }

  /**
   * Try to create an instance of a given class by using whatever constructor is available
   * and trying to instantiate the first parameter recursively if there is a parameter for that constructor.
   * 
   * This is useful to instantiate nested classes which are referencing their outer class in their constructor
   */
  def tryToCreateObject[T <: AnyRef](className: String,
                                     printMessage: Boolean= true,
                                     printStackTrace: Boolean = true,
                                     loader: ClassLoader = Thread.currentThread.getContextClassLoader,
                                     parameter: Option[AnyRef] = None)
                                    (implicit m: Manifest[T]): Option[T] = {

    lazy val canPrintMessage    = printMessage    || sys.props("debugCreateObject") != null
    lazy val canPrintStackTrace = printStackTrace || sys.props("debugCreateObject") != null

    tryToCreateObjectEither(className, loader, parameter) match {
      case Right(o) => Some(o)
      case Left(e)  => {
        if (canPrintMessage) println(e)
        if (canPrintStackTrace) e.getFullStackTrace foreach (s => println(s.toString))
        None
      }
    }
  }

  /**
   * Try to create an instance of a given class by using whatever constructor is available
   * and trying to instantiate the first parameter recursively if there is a parameter for that constructor.
   *
   * This is useful to instantiate nested classes which are referencing their outer class in their constructor
   */
  def tryToCreateObjectEither[T <: AnyRef](className: String,
                                           loader: ClassLoader = Thread.currentThread.getContextClassLoader,
                                           parameter: Option[AnyRef] = None)
                                          (implicit m: Manifest[T]): Either[Throwable, T] = {
    loadClassEither(className, loader) match {
      case Left(e) => Left(e)
      case Right(c: Class[_]) => {
        try {
          val constructors = c.getDeclaredConstructors.toList
          if (constructors.isEmpty)
            Left(new Exception("Can't find a constructor for class "+c.getName))
          else if (constructors.toList(0).getParameterTypes.isEmpty)
            createInstanceOfEither[T](Some[Class[T]](c.asInstanceOf[Class[T]]))
          else if (constructors.toList(0).getParameterTypes.size == 1) {
            // if the specification has a construction, it is either because it is a nested class
            // or if it has an Arguments parameter
            val outerClass = tryToCreateObject[T](getOuterClassName(c), false, false)
            outerClass.orElse(parameter).map(constructors(0).newInstance(_).asInstanceOf[T]).toRight(new Exception("can't create an instance of "+className))
          }
          else {
            Left(new Exception("Can't find a suitable constructor for class "+c.getName))
          }
        } catch {
          case e => Left(new Exception("Could not instantiate class " + className + ": " + e.getMessage, e))
        }
      }
    }
  }
  /**
   * @return an instance of a given class, checking that the created instance typechecks as expected
   */
  private[reflect] def createInstanceOf[T <: AnyRef](c: Option[Class[T]])(implicit m: Manifest[T]): Option[T] = {
    c.map(createInstanceFor(_))
  }
  /**
   * @return an instance of a given class, checking that the created instance typechecks as expected
   */
  private[reflect] def createInstanceOfEither[T <: AnyRef](c: Option[Class[T]])(implicit m: Manifest[T]): Either[Throwable, T] = {
    try { c.map(createInstanceFor(_)).toRight(new Exception()) }
    catch { case e => Left(e) }
  }
  /**
   * @return an instance of a given class, checking that the created instance typechecks as expected
   */
  private[reflect] def createInstanceFor[T <: AnyRef](klass: Class[T])(implicit m: Manifest[T]): T = {
    val constructor = klass.getDeclaredConstructors()(0)
    constructor.setAccessible(true)
  	try {
      var instance: AnyRef = constructor.newInstance().asInstanceOf[AnyRef]
      if (!m.erasure.isInstance(instance)) {
        error(instance + " is not an instance of " + m.erasure.getName)
      }
      instance.asInstanceOf[T]
  	} catch {
  	  case e: java.lang.reflect.InvocationTargetException => throw e.getTargetException
  	}
  }

  /**
   * Load a class, given the class name
   * 
   * If the 'debugLoadClass' property is set, then an error message is printed out to the Console
   */
  private[reflect] def loadClass[T <: AnyRef](className: String, loader: ClassLoader = Thread.currentThread.getContextClassLoader): Option[Class[T]] = {
    loadClassEither(className, loader) match {
      case Right(c) => Some(c.asInstanceOf[Class[T]])
      case Left(e)  => {
        if (sys.props("debugLoadClass") != null) {
          println("Could not load class " + className + ": " + e.getMessage)
          e.getStackTrace foreach (s => println(s.toString))
        }
        None
      }
    }
  }
  /**
   * Load a class, given the class name
   *
   * If the 'debugLoadClass' property is set, then an error message is printed out to the Console
   */
  private[reflect] def loadClassEither[T <: AnyRef](className: String, loader: ClassLoader = Thread.currentThread.getContextClassLoader):
    Either[Throwable, Class[T]] = {
    trye(loadClassOf(className, loader).asInstanceOf[Class[T]])
  }
  /**
   * Load a class, given the class name, without catching exceptions
   */
  private[specs2] def loadClassOf[T <: AnyRef](className: String = "", loader: ClassLoader = Thread.currentThread.getContextClassLoader): Class[T] = {
    loader.loadClass(className).asInstanceOf[Class[T]]
  }
}
/**
 * This object provides simple functions to instantiate classes.
 */
private[specs2]
object Classes extends Classes with ConsoleOutput
