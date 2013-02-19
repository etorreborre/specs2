package org.specs2
package execute

/**
 * This class is used to delay the execution of a result
 */
class AnyAsResult(var t: () => Result) {
  def :=[R : AsResult](r: =>R) = {
    t = () => AsResult(r)
    this
  }
}

object AnyAsResult {
  implicit def anyResultAsResult[T]: AsResult[AnyAsResult] = new AsResult[AnyAsResult] {
    def asResult(code: =>AnyAsResult): Result = code.t()
  }
  implicit def anyToAnyResult[T : AsResult](t: =>T): AnyAsResult =
    new AnyAsResult(() => AsResult(t))
}

