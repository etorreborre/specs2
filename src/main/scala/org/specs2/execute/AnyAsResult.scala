package org.specs2
package execute

/**
 * This class is used to delay the
 */
case class AnyAsResult(t: () => Result)

object AnyAsResult {
  implicit def anyResultAsResult[T]: AsResult[AnyAsResult] = new AsResult[AnyAsResult] {
    def asResult(code: =>AnyAsResult): Result = code.t()
  }
  implicit def anyToAnyResult[T : AsResult](t: =>T): AnyAsResult = AnyAsResult(() => AsResult(t))
}

