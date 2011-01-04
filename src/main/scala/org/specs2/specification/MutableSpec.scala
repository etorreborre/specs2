package org.specs2
package specification
import execute._

/**
 * Adding new implicits to support specs-like naming
 */
trait MutableSpec extends BaseSpecification {
  import FormattingFragments._
  private var specFragments: Fragments = new Fragments()

  def is = specFragments

  implicit def described(s: String): Described = new Described(s)
  class Described(s: String) {
    def should(fs: =>Example) = addFragments(s, fs, "should")
    def can(fs: =>Example) = addFragments(s, fs, "can")
  }
  protected def addFragments[T](s: String, fs: =>T, word: String) {
    specFragments = specFragments ^ s + " " + word
    fs
    specFragments = specFragments ^ p
  }
  protected def addExample[T <% Result](s: String, r: =>T): Example = {
    val ex = s ! r
    specFragments = specFragments ^ ex
    ex
  }
  implicit def inExample(s: String): InExample = new InExample(s)
  class InExample(s: String) {
    def in[T <% Result](r: =>T) = addExample(s, r)
    def >>[T <% Result](r: =>T) = in(r)
    def >>(e: =>Example) = addFragments(s, e, "")
  }

}