package org.specs2
package reflect

import scala.reflect.ClassManifest
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
                         (implicit m: ClassManifest[T]): Either[Throwable, T] =
    trye(createInstanceFor(loadClassOf[T](className, loader = classLoader)))

  /** @return an instance of a given class */
  def createObject[T <: AnyRef](className: String)(implicit m: ClassManifest[T]): Option[T] =
    createObject[T](className, false)(m)
  
  /** @return an instance of a given class and optionally print message if the class can't be loaded */
  def createObject[T <: AnyRef](className: String, printMessage: Boolean)
                               (implicit m: ClassManifest[T]): Option[T] =
    createObject(className, printMessage, false)(m)

  /**
   * A system property 'debugCreateObject' can be set to override the printMessage and printStackTrace parameters
   * so that the exception message and stacktrace are printed when the object can't be created
   * 
   * @return an instance of a given class and optionally print message and/or the stacktrace if the class can't be loaded.
   */
  def createObject[T <: AnyRef](className: String, printMessage: Boolean, printStackTrace: Boolean)
                               (implicit m: ClassManifest[T]): Option[T] = {
    tryo(createInstanceOf[T](loadClass[T](className))) { (e: Exception) =>
      val debugCreateObject = System.getProperty("debugCreateObject") != null
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
  def tryToCreateObject[T <: AnyRef](className: String, printMessage: Boolean= true, printStackTrace: Boolean = true,
                                     loader: ClassLoader = Thread.currentThread.getContextClassLoader)
                                    (implicit m: ClassManifest[T]): Option[T] = {
    loadClass(className) match {
      case None => None
      case Some(c: Class[_]) => {
        try {
          val constructors = c.getDeclaredConstructors.toList
          if (constructors.isEmpty)
            None
          else if (constructors.toList(0).getParameterTypes.isEmpty)
            createInstanceOf[T](Some[Class[T]](c.asInstanceOf[Class[T]]))
          else if (constructors.toList(0).getParameterTypes.size == 1) {
            val outerClassName = getOuterClassName(c)
            tryToCreateObject[T](outerClassName, printMessage, printStackTrace).map(constructors(0).newInstance(_).asInstanceOf[T])
          }
          else
            None
        } catch {
          case e => {
            if (printMessage || System.getProperty("debugCreateObject") != null) println("Could not instantiate class " + className + ": " + e)
            if (printStackTrace || System.getProperty("debugCreateObject") != null) e.getFullStackTrace foreach (s => println(s.toString))
            return None
          }
        }
      }
    }
  }
  /**
   * @return an instance of a given class, checking that the created instance typechecks as expected
   */
  private[reflect] def createInstanceOf[T <: AnyRef](c: Option[Class[T]])(implicit m: ClassManifest[T]): Option[T] = {
    c.map(createInstanceFor(_))
  }
  /**
   * @return an instance of a given class, checking that the created instance typechecks as expected
   */
  private[reflect] def createInstanceFor[T <: AnyRef](klass: Class[T])(implicit m: ClassManifest[T]) = {
    val constructor = klass.getDeclaredConstructors()(0)
    constructor.setAccessible(true)
  	try {
      val instance: AnyRef = constructor.newInstance().asInstanceOf[AnyRef]
      if (!m.erasure.isInstance(instance))
        error(instance + " is not an instance of " + m.erasure.getName)
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
    tryo(Some(loadClassOf(className).asInstanceOf[Class[T]])) { (e: Throwable) =>
      if (System.getProperty("debugLoadClass") != null) {
        println("Could not load class " + className + ": " + e.getMessage)
        e.getStackTrace() foreach (s => println(s.toString))
      }
    }.flatMap(identity)
  }
  /**
   * Load a class, given the class name, without catching exceptions
   */
  private[reflect] def loadClassOf[T <: AnyRef](className: String = "", loader: ClassLoader = Thread.currentThread.getContextClassLoader): Class[T] = {
    loader.loadClass(className).asInstanceOf[Class[T]]
  }
}
/**
 * This object provides simple functions to instantiate classes.
 */
private[specs2]
object Classes extends Classes with ConsoleOutput
