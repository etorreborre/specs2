package org.specs2
package main

import control.Exceptions._
import reflect.Classes

import scala.reflect.ClassTag
import scalaz.Memo
import scalaz.Memo._

/**
 * Extraction functions for command line parameters
 */
trait Extract {

  /**
   * memoize the boolean properties to improve performances
   */
  private val booleanProperties = immutableHashMapMemo[(String, SystemProperties), Option[Boolean]] { case (name, sp) =>
    sp.getPropertyAs[Boolean](name) orElse sp.getProperty(name).map(v => true)
  }

  def boolSystemProperty(name: String)(implicit sp: SystemProperties): Option[Boolean] = booleanProperties(name -> sp)

  def bool(name: String, mappedValue: Boolean = true)(implicit args: Seq[String], sp: SystemProperties): Option[Boolean] = {
    args.find(_.toLowerCase.contains(name.toLowerCase)).map(a => mappedValue).orElse(boolSystemProperty(name))
  }

  def bool(name: String, negatedName: String)(implicit args: Seq[String], sp: SystemProperties): Option[Boolean] = {
    bool(negatedName, false) orElse bool(name)
  }
  def value[T](name: String, f: String => T)(implicit args: Seq[String], sp: SystemProperties): Option[T] = {
    args.zip(args.drop(1)).find(_._1.toLowerCase == name.toLowerCase).map(s => f(s._2)).orElse(valueSystemProperty(name, f))
  }

  def valueSystemProperty[T](name: String, f: String => T)(implicit sp: SystemProperties): Option[T] = {
    sp.getProperty(name).map(o => f(o.toString))
  }

  def value[T](name: String)(implicit args: Seq[String], sp: SystemProperties): Option[String] = value(name, identity)

  def int(name: String)(implicit args: Seq[String], sp: SystemProperties): Option[Int] = {
    tryo(value(name)(args, sp).map(_.toInt)).getOrElse(None)
  }

  def long(name: String)(implicit args: Seq[String], sp: SystemProperties): Option[Long] = {
    tryo(value(name)(args, sp).map(_.toLong)).getOrElse(None)
  }

  def instance[T <: AnyRef](name: String)(implicit m: ClassTag[T]): Option[T] =
    Classes.createInstance[T](name, getClass.getClassLoader).runOption

}

