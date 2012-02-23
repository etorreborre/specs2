package org.specs2
package examples
import specification._

class SlowFragsSpec extends Specification {

  def is = "concatenating fragments is slow" ^ Fragments.createList(frags: _*)

  def frags: Seq[Fragment]      = foo.flatMap(_.fragments)
  def foo: Seq[Fragments]       = for (i <- 1 to 50) yield "foo %s".format(i) ^ bar(i)
  def bar(i: Int): Seq[Example] = for (j <- 1 to 20) yield "bar %s %s".format(i, j) ! success
}
