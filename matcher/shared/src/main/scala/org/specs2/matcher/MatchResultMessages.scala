package org.specs2
package matcher

import org.specs2.fp._

/**
 * MatchResultMessages are used to accumulate the results of several matches into a "summary" one
 */
private[specs2]
trait MatchResultMessages {

  implicit val MatchResultMessageMonoid: Monoid[MatchResultMessage] = new Monoid[MatchResultMessage] {
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
    lazy val okMessage = ok()
    lazy val koMessage = ko()

    def append(m2: MatchResultMessage) = {
      m2 match {
        case s: MatchSuccess[_] => SuccessMessage.create(okMessage+"; "+s.okMessage, koMessage+"; "+s.koMessage)
        case f: MatchFailure[_] => FailureMessage.create(okMessage+"; "+f.okMessage, koMessage+"; "+f.koMessage)
        case NeutralMessage(m)  => SuccessMessage.create(okMessage+"; "+m, koMessage+"; "+m)
        case _                  => this
      }
    }
  }
  object SuccessMessage {
    def create(ok: =>String, ko: =>String) = new SuccessMessage(() => ok, () => ko)
  }
  case class FailureMessage(ok: () => String, ko: () => String) extends MatchResultMessage {
    lazy val okMessage = ok()
    lazy val koMessage = ko()

    def append(m2: MatchResultMessage) = {
      m2 match {
        case s: MatchSuccess[_] => FailureMessage.create(okMessage+"; "+s.okMessage, koMessage+"; "+s.koMessage)
        case f: MatchFailure[_] => FailureMessage.create(okMessage+"; "+f.okMessage, koMessage+"; "+f.koMessage)
        case NeutralMessage(m)  => FailureMessage.create(okMessage+"; "+m, koMessage+"; "+m)
        case _                  => this
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
        case s: MatchSuccess[_] => SuccessMessage.create(message+"; "+s.okMessage, message+"; "+s.koMessage)
        case f: MatchFailure[_] => FailureMessage.create(message+"; "+f.okMessage, message+"; "+f.koMessage)
        case NeutralMessage(m)  => NeutralMessage(message+"; "+m)
        case _                  => this
      }
    }
  }
  case class EmptySuccessMessage() extends MatchResultMessage {
    def append(m2: MatchResultMessage) = m2
  }
}
private[specs2]
object MatchResultMessages extends MatchResultMessages
