package org.specs2
package shapeless

import Projection._
import org.specs2.execute._, Typecheck._
import org.specs2.matcher.TypecheckMatchers._

class ProjectionSpec extends Specification { def is = s2"""

  project on Empty       $s1
  project on itself      $s2
  project on "subset"    $s3
  project recursively    $s4
  do not project anyvals $s5

  """

  case class A(i: Int, d: Double, c: Char, b: B)
  case class B(i: Int, c: Char)
  case class C(c: Char)
  case class D(i: Int, c: C)
  case class Empty()

  def s1 = {
    A(1, 2.0, 'c', B(1, 'c')).projectOn[Empty] must be equalTo Empty()
  }

  def s2 = {
    A(1, 2.0, 'c', B(1, 'c')).projectOn[A] must be equalTo A(1, 2.0, 'c', B(1, 'c'))
  }

  def s3 = {
    A(1, 2.0, 'c', B(1, 'c')).projectOn[B] must be equalTo B(1, 'c')
  }

  def s4 = {
    A(1, 2.0, 'c', B(1, 'c')).projectOn[D] must be equalTo D(1, C('c'))
  }

  def s5 = {
    typecheck {
      """F("test").projectOn[G]"""
    } must
      failWith(""".*could not find implicit value for parameter projection.*""")

  }
}

case class F(s: String) extends AnyVal
case class G(s: String) extends AnyVal
