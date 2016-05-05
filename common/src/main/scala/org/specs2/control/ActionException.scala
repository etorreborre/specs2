package org.specs2
package control

/**
 * This exception class is used when an Action is transformed to a Task to avoid losing information
 */
case class ActionException(warnings: List[String], message: Option[String], throwable: Option[Throwable]) extends Exception {
  override def getMessage: String =
    (if (warnings.nonEmpty) warnings.mkString("Warnings:\n", "\n", "\n") else "") +
    message.getOrElse("")
}

/**
 * This exception class is used to notify the user of instantiation errors
 */
case class UserException(message: String, throwable: Throwable) extends Exception(message, throwable)


