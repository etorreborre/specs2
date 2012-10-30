package org.specs2
package matcher

import org.specs2.internal.scalaz.{Monoid, Reducer}

/**
 * MatchResultMessages are used to accumulate the results of several matches into a "summary" one
 */
private[specs2]
trait MatchResultMessages {

  implicit def MatchResultMessageReducer[T] = new Reducer[MatchResult[T], MatchResultMessage] {
    override def unit(r: MatchResult[T]) = r match {
      case MatchSuccess(ok, ko, e)    => SuccessMessage.create(ok(), ko())
      case MatchFailure(ok, ko, e, d) => FailureMessage.create(ok(), ko())
      case _                          => NeutralMessage(r.message)
    }
  }
  implicit val MatchResultMessageMonoid = new Monoid[MatchResultMessage] {
    val zero = new EmptySuccessMessage()
    def append(r1: MatchResultMessage, r2: =>MatchResultMessage) = r1 append r2
  }

  /**
   * A MatchResultMessage represents an accumulation of messages from different matches.
   *
   * It can be appended another MatchResultMessage so that there is a Semigroup for this class
   */
  sealed trait MatchResultMessage {
    def append(m2: MatchResultMessage): MatchResultMessage
    def isSuccess: Boolean = true
  }
  case class SuccessMessage(ok: () => String, ko: () => String) extends MatchResultMessage {
    def okMessage = ok()
    def koMessage = ko()

    def append(m2: MatchResultMessage) = {
      m2 match {
        case SuccessMessage(ok, ko) => SuccessMessage.create(okMessage+"; "+ok(), koMessage+"; "+ko())
        case FailureMessage(ok, ko) => FailureMessage.create(okMessage+"; "+ok(), koMessage+"; "+ko())
        case NeutralMessage(m)      => SuccessMessage.create(okMessage+"; "+m, koMessage+"; "+m)
        case _                      => this
      }
    }
  }
  object SuccessMessage {
    def create(ok: =>String, ko: =>String) = new SuccessMessage(() => ok, () => ko)
  }
  case class FailureMessage(ok: () => String, ko: () => String) extends MatchResultMessage {
    def okMessage = ok()
    def koMessage = ko()

    def append(m2: MatchResultMessage) = {
      m2 match {
        case SuccessMessage(ok, ko) => FailureMessage.create(okMessage+"; "+ok(), koMessage+"; "+ko())
        case FailureMessage(ok, ko) => FailureMessage.create(okMessage+"; "+ok(), koMessage+"; "+ko())
        case NeutralMessage(m)      => FailureMessage.create(okMessage+"; "+m, koMessage+"; "+m)
        case _                      => this
      }
    }
    override def isSuccess: Boolean = false
  }
  object FailureMessage {
    def create(ok: =>String, ko: =>String) = new FailureMessage(() => ok, () => ko)
  }
  case class NeutralMessage(message: String) extends MatchResultMessage {
    def append(m2: MatchResultMessage) = {
      m2 match {
        case SuccessMessage(ok, ko) => SuccessMessage.create(message+"; "+ok, message+"; "+ko())
        case FailureMessage(ok, ko) => FailureMessage.create(message+"; "+ok(), message+"; "+ko)
        case NeutralMessage(m)      => NeutralMessage(message+"; "+m)
        case _ => this
      }
    }
  }
  case class EmptySuccessMessage() extends MatchResultMessage {
    def append(m2: MatchResultMessage) = m2
  }
}
private[specs2]
object MatchResultMessages extends MatchResultMessages
