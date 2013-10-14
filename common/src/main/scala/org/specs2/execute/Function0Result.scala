package org.specs2
package execute

/**
 * This class is used to delay the execution of a result
 */
class Function0Result(var t: () => Result) {
  def :=[R : AsResult](r: =>R) = {
    t = () => AsResult(r)
    this
  }
}

object Function0Result {
  implicit def anyResultAsResult[T]: AsResult[Function0Result] = new AsResult[Function0Result] {
    def asResult(code: =>Function0Result): Result = code.t()
  }
  implicit def anyToAnyResult[T : AsResult](t: =>T): Function0Result =
    new Function0Result(() => AsResult(t))

}

