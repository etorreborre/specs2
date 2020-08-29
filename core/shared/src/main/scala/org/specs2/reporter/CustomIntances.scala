package org.specs2
package reporter

import main._
import control._
import fp.syntax._
import reflect.Classes
import scala.reflect.ClassTag
import Printer._

case class CustomInstances(arguments: Arguments, loader: ClassLoader, logger: Logger):

  /** create a built-in specs2 printer */
  def createPrinterInstance(name: PrinterName, className: String, failureMessage: String, noRequiredMessage: String): Operation[Option[Printer]] =
    if arguments.isSet(name.name) then createInstance[Printer](name.name, className, _ => failureMessage, noRequiredMessage)
    else noInstance(noRequiredMessage)

  /** create a custom instance */
  def createCustomInstance[T <: AnyRef](name: String, failureMessage: String => String, noRequiredMessage: String)(implicit m: ClassTag[T]): Operation[Option[T]] =
    arguments.commandLine.value(name) match
      case Some(className) => createInstance[T](name, className, failureMessage, noRequiredMessage)
      case None => noInstance(noRequiredMessage)

  private def createInstance[T <: AnyRef](name: String, className: String, failureMessage: String => String, noRequiredMessage: String)(implicit m: ClassTag[T]): Operation[Option[T]] =
    for
      instance <- Classes.createInstanceEither[T](className, loader)(m)
      result <-
        instance match
          case Right(i) => Operation.ok(Option(i))
          case Left(t) => noInstanceWithException[T](failureMessage(t.getMessage), t, forceVerbose = Some(true))
    yield result

  /** print a message if a class can not be instantiated */
  def noInstanceWithException[T](message: String, t: Throwable, forceVerbose: Option[Boolean] = None): Operation[Option[T]] =
    val verbose = forceVerbose.getOrElse(arguments.verbose)
    logger.info("", verbose) >>
      logger.info(message, verbose) >>
      logger.info("", verbose) >>
      logger.exception(t, verbose) >>
      Operation.ok(None)

  /** print a message if a class can not be instantiated */
  def noInstance[T](message: String): Operation[Option[T]] =
    logger.info(message, arguments.verbose) >> Operation.ok(None)


object CustomInstances:

  def default: CustomInstances =
    create(Arguments())

  def create(args: Arguments, logger: Logger = NoLogger): CustomInstances =
    val loader = Thread.currentThread.getContextClassLoader
    CustomInstances(args, loader, logger)
