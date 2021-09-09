package org.specs2
package main

import control.Exceptions.*
import org.specs2.text.FromString
import reflect.Classes

import scala.reflect.ClassTag
import org.specs2.fp.Memo.*

/** Extraction functions for command line parameters
  */
trait Extract:

  /** memoize the boolean properties to improve performances
    */
  private val booleanProperties = immutableHashMapMemo[(String, SystemProperties), Option[Boolean]] { case (name, sp) =>
    sp.getPropertyAs[Boolean](name) orElse sp.getProperty(name).map(v => true)
  }

  def boolSystemProperty(name: String)(using sp: SystemProperties): Option[Boolean] = booleanProperties(name -> sp)

  def boolValue(name: String, negate: Boolean = false)(using args: Seq[String], sp: SystemProperties): Option[Boolean] =
    val booleanValue =
      value(name).flatMap(FromString[Boolean].fromString) orElse // if the flag is defined with a value
        args
          .find(_.toLowerCase == name.toLowerCase)
          .map(_ => true) orElse // if the flag is defined as a switch on the commandline
        boolSystemProperty(name).map(_ => true) // if the flag is defined as a switch in sys. properties

    if negate then booleanValue.map(b => !b)
    else booleanValue

  def bool(name: String)(using args: Seq[String], sp: SystemProperties): Option[Boolean] =
    bool(name, "!" + name)

  def bool(name: String, negatedName: String)(using args: Seq[String], sp: SystemProperties): Option[Boolean] =
    boolValue(negatedName, negate = true) orElse boolValue(name)

  def value[T](name: String, f: String => T)(using args: Seq[String], sp: SystemProperties): Option[T] =
    args
      .zip(args.drop(1))
      .find(_._1.toLowerCase == name.toLowerCase)
      .map(s => f(s._2))
      .orElse(valueSystemProperty(name, f))

  def valueSystemProperty[T](name: String, f: String => T)(using sp: SystemProperties): Option[T] =
    sp.getProperty(name).map(o => f(o.toString))

  def value[T](name: String)(using args: Seq[String], sp: SystemProperties): Option[String] =
    value(name, identity)

  def int(name: String)(using args: Seq[String], sp: SystemProperties): Option[Int] =
    tryo(value(name).map(_.toInt)).getOrElse(None)

  def long(name: String)(using args: Seq[String], sp: SystemProperties): Option[Long] =
    tryo(value(name).map(_.toLong)).getOrElse(None)

  def double(name: String)(using args: Seq[String], sp: SystemProperties): Option[Double] =
    tryo(value(name).map(_.toDouble)).getOrElse(None)

  def float(name: String)(using args: Seq[String], sp: SystemProperties): Option[Float] =
    tryo(value(name).map(_.toFloat)).getOrElse(None)

  def instance[T <: AnyRef](name: String)(using m: ClassTag[T]): Option[T] =
    Classes.createInstanceFromName[T](name).runOption

sealed trait ArgumentType:
  def name: String

case class BooleanArgument(name: String) extends ArgumentType
case class ValuedArgument(name: String) extends ArgumentType
