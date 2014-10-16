package org.specs2
package control

/**
 * This exception class is used when an Action is transformed to a Task to avoid losing information
 */
case class ActionException(warnings: Vector[String], message: Option[String], throwable: Option[Throwable]) extends Exception {
  override def getMessage: String =
    (if (warnings.nonEmpty) warnings.mkString("Warnings:\n", "\n", "\n") else "") +
    message.getOrElse("")
}


