package org.specs2
package main

import org.specs2.concurrent.ExecuteArguments
import scala.concurrent.duration.*

/** Execution arguments
  */
case class Execute(
    _plan: Option[Boolean] = None,
    _skipAll: Option[Boolean] = None,
    _stopOnFail: Option[Boolean] = None,
    _stopOnError: Option[Boolean] = None,
    _stopOnIssue: Option[Boolean] = None,
    _stopOnSkip: Option[Boolean] = None,
    _sequential: Option[Boolean] = None,
    _sequentialRandom: Option[Boolean] = None,
    _asap: Option[Boolean] = None,
    _useCustomClassLoader: Option[Boolean] = None,
    _threadsNb: Option[Int] = None,
    _specs2ThreadsNb: Option[Int] = None,
    _scheduledThreadsNb: Option[Int] = None,
    _discardRejectedFutures: Option[Boolean] = None,
    _batchSize: Option[Int] = None,
    _timeFactor: Option[Int] = None,
    _timeout: Option[FiniteDuration] = None,
    _executor: Option[String] = None
) extends ShowArgs:

  def plan: Boolean = _plan.getOrElse(false)
  def skipAll: Boolean = _skipAll.getOrElse(false)
  def stopOnFail: Boolean = _stopOnFail.getOrElse(false)
  def stopOnError: Boolean = _stopOnError.getOrElse(false)
  def stopOnIssue: Boolean = _stopOnIssue.getOrElse(false)
  def stopOnSkip: Boolean = _stopOnSkip.getOrElse(false)
  def sequential: Boolean = _sequential.getOrElse(false)
  def sequentialRandom: Boolean = _sequentialRandom.getOrElse(false)
  def asap: Boolean = _asap.getOrElse(false)
  def useCustomClassLoader: Boolean = _useCustomClassLoader.getOrElse(false)
  def threadsNb: Int = _threadsNb.getOrElse(ExecuteArguments.threadsNb)
  def specs2ThreadsNb: Int = _specs2ThreadsNb.getOrElse(ExecuteArguments.specs2ThreadsNb)
  def scheduledThreadsNb: Int = _scheduledThreadsNb.getOrElse(1)
  def discardRejectedFutures: Boolean = _discardRejectedFutures.getOrElse(true)
  def batchSize: Int = _batchSize.getOrElse(ExecuteArguments.threadsNb)
  def timeFactor: Int = _timeFactor.getOrElse(1)
  def timeout: Option[FiniteDuration] = _timeout
  def setTimeout(t: FiniteDuration) = copy(_timeout = Some(t))
  def executor: Option[String] = _executor

  def overrideWith(other: Execute) =
    new Execute(
      other._plan.orElse(_plan),
      other._skipAll.orElse(_skipAll),
      other._stopOnFail.orElse(_stopOnFail),
      other._stopOnError.orElse(_stopOnError),
      other._stopOnIssue.orElse(_stopOnIssue),
      other._stopOnSkip.orElse(_stopOnSkip),
      other._sequential.orElse(_sequential),
      other._sequentialRandom orElse (_sequentialRandom),
      other._asap.orElse(_asap),
      other._useCustomClassLoader.orElse(_useCustomClassLoader),
      other._threadsNb.orElse(_threadsNb),
      other._specs2ThreadsNb.orElse(_specs2ThreadsNb),
      other._scheduledThreadsNb.orElse(_scheduledThreadsNb),
      other._discardRejectedFutures.orElse(_discardRejectedFutures),
      other._batchSize.orElse(_batchSize),
      other._timeFactor.orElse(_timeFactor),
      other._timeout.orElse(_timeout),
      other._executor.orElse(_executor)
    )

  override def toString =
    List(
      "plan" -> _plan,
      "skipAll" -> _skipAll,
      "stopOnFail" -> _stopOnFail,
      "stopOnError" -> _stopOnError,
      "stopOnIssue" -> _stopOnIssue,
      "stopOnSkip" -> _stopOnSkip,
      "sequential" -> _sequential,
      "sequentialRandom" -> _sequentialRandom,
      "asap" -> _asap,
      "useCustomClassLoader" -> _useCustomClassLoader,
      "threadsNb" -> _threadsNb,
      "specs2ThreadsNb" -> _specs2ThreadsNb,
      "scheduledThreadsNb" -> _scheduledThreadsNb,
      "discardRejectedFutures" -> _discardRejectedFutures,
      "batchSize" -> _batchSize,
      "timeFactor" -> _timeFactor,
      "timeout" -> _timeout,
      "executor" -> _executor
    ).flatMap(showArg).mkString("Execute(", ", ", ")")

object Execute extends Extract:
  def extract(using arguments: Seq[String], systemProperties: SystemProperties): Execute =
    new Execute(
      _plan = bool("plan"),
      _skipAll = bool("skipAll"),
      _stopOnFail = bool("stopOnFail"),
      _stopOnError = bool("stopOnError"),
      _stopOnIssue = bool("stopOnIssue"),
      _stopOnSkip = bool("stopOnSkip"),
      _sequential = bool("sequential"),
      _sequentialRandom = bool("sequentialRandom"),
      _asap = bool("asap"),
      _useCustomClassLoader = bool("useCustomClassLoader"),
      _threadsNb = int("threadsNb"),
      _specs2ThreadsNb = int("specs2ThreadsNb"),
      _scheduledThreadsNb = int("scheduledThreadsNb"),
      _discardRejectedFutures = bool("discardRejectedFutures"),
      _batchSize = bool("unbatched").map(_ => Int.MaxValue).orElse(int("batchSize")),
      _timeFactor = int("timeFactor"),
      _timeout = int("timeout").map(_.millis),
      _executor = value("executor")
    )

  val allArguments: Seq[ArgumentType] =
    Seq(
      BooleanArgument("plan"),
      BooleanArgument("skipAl"),
      BooleanArgument("stopOnFail"),
      BooleanArgument("stopOnError"),
      BooleanArgument("stopOnIssue"),
      BooleanArgument("stopOnSkip"),
      BooleanArgument("sequential"),
      BooleanArgument("asap"),
      BooleanArgument("isolated"),
      BooleanArgument("useCustomClassLoader"),
      ValuedArgument("threadsNb"),
      ValuedArgument("specs2ThreadsNb"),
      ValuedArgument("discardRejectedFutures"),
      BooleanArgument("unbatched"),
      ValuedArgument("batchSize"),
      ValuedArgument("timeFactor"),
      ValuedArgument("timeFactor"),
      ValuedArgument("executor")
    )
