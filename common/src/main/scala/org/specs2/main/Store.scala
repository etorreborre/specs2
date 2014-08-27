package org.specs2
package main

/**
 * Storing arguments
 */
case class Store(_reset:         Option[Boolean]          = None,
                 _never:         Option[Boolean]          = None) extends ShowArgs {

  def reset: Boolean = _reset.getOrElse(false)
  def never: Boolean = _never.getOrElse(false)

  def overrideWith(other: Store) = {
    new Store(
      other._reset.orElse(_reset),
      other._never.orElse(_never)
    )
  }

  override def toString =
    List(
      "reset" -> _reset      ,
      "never" -> _never      ).flatMap(showArg).mkString("Store(", ", ", ")")

}

object Store extends Extract {
  def extract(implicit arguments: Seq[String], systemProperties: SystemProperties): Store = {
    new Store (
      _reset = bool("resetStore"),
      _never = bool("neverStore")
    )
  }

  val allValueNames = Seq("resetStore", "neverStore")
}

