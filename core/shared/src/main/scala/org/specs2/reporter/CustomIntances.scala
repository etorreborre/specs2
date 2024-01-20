package org.specs2
package reporter

import main.*
import control.*
import specification.core.*
import fp.syntax.*
import reflect.Classes
import scala.reflect.ClassTag
import Printer.*

case class CustomInstances(env: Env, loader: ClassLoader, logger: Logger):

  /** create a built-in specs2 printer */
  def createPrinterInstance(
      name: PrinterName,
      className: String,
      failureMessage: String,
      noRequiredMessage: String
  ): Operation[Option[Printer]] =
    if env.arguments.isSet(name.name) then
      createInstance[Printer](name.name, className, _ => failureMessage, noRequiredMessage)
    else noInstance(noRequiredMessage)

  /** create a custom instance */
  def createCustomInstance[T <: AnyRef](name: String, failureMessage: String => String, noRequiredMessage: String)(using
      m: ClassTag[T]
  ): Operation[Option[T]] =
    env.arguments.commandLine.value(name) match
      case Some(className) => createInstance[T](name, className, failureMessage, noRequiredMessage)
      case _               => noInstance(noRequiredMessage)

  private def createInstance[T <: AnyRef](
      name: String,
      className: String,
      failureMessage: String => String,
      noRequiredMessage: String
  )(using m: ClassTag[T]): Operation[Option[T]] =
    for
      instance <- Classes.createInstanceEither[T](className, loader, env.defaultInstances)
      result <-
        instance match
          case Right(i) => Operation.ok(Option(i))
          case Left(t)  => noInstanceWithException[T](failureMessage(t.getMessage), t, forceVerbose = Some(true))
    yield result

  /** print a message if a class can not be instantiated */
  def noInstanceWithException[T](
      message: String,
      t: Throwable,
      forceVerbose: Option[Boolean] = None
  ): Operation[Option[T]] =
    val verbose = forceVerbose.getOrElse(env.arguments.verbose)
    logger.info("", verbose) >>
      logger.info(message, verbose) >>
      logger.info("", verbose) >>
      logger.exception(t, verbose) >>
      Operation.ok(None)

  /** print a message if a class can not be instantiated */
  def noInstance[T](message: String): Operation[Option[T]] =
    logger.info(message, env.arguments.verbose) >> Operation.ok(None)

object CustomInstances:

  def default: CustomInstances =
    create(EnvDefault.default)

  def create(env: Env, logger: Logger = NoLogger): CustomInstances =
    val loader = Thread.currentThread.getContextClassLoader
    CustomInstances(env, loader, logger)
