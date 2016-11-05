package org.specs2.control.eff.syntax

import org.specs2.control.eff._

trait async extends
  AsyncFutureServiceInterpretation with
  AsyncInterpretation

object async extends async
