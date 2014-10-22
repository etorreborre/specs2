package user


import org.specs2.Specification
import org.specs2.matcher.MatcherMacros

class TestSpec extends Specification with MatcherMacros { def is = s2"""

 Special match in a map $map

"""

  def map = {
    val m = Map[String, A]("key1" -> new A { def x = 3; def y = 4 })

    get(m, "key1") must beSome(matchAn[A].x(1).y(2))
  }

  def get(m: Map[String, A], key: String) = m.get(key) aka key

}
trait A {
  def x: Int
  def y: Int
}

