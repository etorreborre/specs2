package org.specs2

import scala.concurrent.ExecutionContext

package object concurrent {

  private[concurrent] def parasitic = ExecutionContext.parasitic

}
