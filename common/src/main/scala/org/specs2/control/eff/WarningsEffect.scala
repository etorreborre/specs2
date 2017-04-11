package org.specs2.control.eff

object WarningsEffect {

  case class WarningsMessage(value: String) extends AnyVal

  type _warnings[R] = Warnings |= R
  type Warnings[A] = Writer[WarningsMessage, A]


  /** warn the user about something that is probably wrong on his side, this is not a specs2 bug */
  def warn[R :_warnings](message: String): Eff[R, Unit] =
    WriterEffect.tell(WarningsMessage(message))

  def runWarnings[R, U, A](w: Eff[R, A])(implicit m: Member.Aux[Warnings, R, U]): Eff[U, (A, List[String])] =
    WriterEffect.runWriter(w).map { case (a, ls) => (a, ls.map(_.value)) }

}

