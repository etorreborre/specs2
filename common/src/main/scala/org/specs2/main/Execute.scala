package org.specs2
package main

/**
 * Execution arguments
 */
case class Execute(
                    _plan:          Option[Boolean]          = None,
                    _skipAll:       Option[Boolean]          = None,
                    _stopOnFail:    Option[Boolean]          = None,
                    _stopOnSkip:    Option[Boolean]          = None,
                    _sequential:    Option[Boolean]          = None,
                    _asap:          Option[Boolean]          = None,
                    _isolated:      Option[Boolean]          = None,
                    _threadsNb:     Option[Int]              = None,
                    _executor:      Option[String]           = None) extends ShowArgs {

  def plan: Boolean                 = _plan.getOrElse(false)
  def skipAll: Boolean              = _skipAll.getOrElse(false)
  def stopOnFail: Boolean           = _stopOnFail.getOrElse(false)
  def stopOnSkip: Boolean           = _stopOnSkip.getOrElse(false)
  def sequential: Boolean           = _sequential.getOrElse(false)
  def asap: Boolean                 = _asap.getOrElse(false)
  def isolated: Boolean             = _isolated.getOrElse(false)
  def threadsNb: Int                = _threadsNb.getOrElse(Runtime.getRuntime.availableProcessors)
  def executor: String              = _executor.getOrElse("")

  def overrideWith(other: Execute) = {
    new Execute(
      other._plan            .orElse(_plan),
      other._skipAll         .orElse(_skipAll),
      other._stopOnFail      .orElse(_stopOnFail),
      other._stopOnSkip      .orElse(_stopOnSkip),
      other._sequential      .orElse(_sequential),
      other._asap            .orElse(_asap),
      other._isolated        .orElse(_isolated),
      other._threadsNb       .orElse(_threadsNb),
      other._executor        .orElse(_executor)
    )
  }

  override def toString =
    List(
      "plan"           -> _plan         ,
      "skipAll"        -> _skipAll      ,
      "stopOnFail"     -> _stopOnFail   ,
      "stopOnSkip"     -> _stopOnSkip   ,
      "sequential"     -> _sequential   ,
      "asap"           -> _asap         ,
      "isolated"       -> _isolated     ,
      "threadsNb"      -> _threadsNb    ,
      "executor"       -> _executor     ).flatMap(showArg).mkString("Execute(", ", ", ")")

}

object Execute extends Extract {
  def extract(implicit arguments: Seq[String], systemProperties: SystemProperties): Execute = {
    new Execute (
      _plan          = bool("plan"),
      _skipAll       = bool("skipAll"),
      _stopOnFail    = bool("stopOnFail"),
      _stopOnSkip    = bool("stopOnSkip"),
      _sequential    = bool("sequential"),
      _asap          = bool("asap"),
      _isolated      = bool("isolated"),
      _threadsNb     = int("threadsNb"),
      _executor      = value("executor")
    )
  }
  val allValueNames = Seq("plan", "skipAll", "stopOnFail", "stopOnSkip", "sequential", "asap", "isolated", "threadsNb", "executor")
}
