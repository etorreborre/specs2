package examples
import org.junit._
import org.specs2.matcher._

/**
 * This class shows how to use specs2 matchers in a JUnit specification
 */
class JUnitTest extends JUnitMustMatchers {

  @Test
  def test1() {
    "Hello world" must have size(11)
  }

  @Test
  def test2() {
    "Hello world" must startWith("Hello")
  }

  @Test
  def test3() {
    "Hello world" must endWith("world")
  }

  @Test
  def test4() {
    "Hello world" must endWithWorld
  }

  @Test
  def containExactlyBeLike() {
    Set(Some("a"),Some("b")) must contain(exactly(beLike[Option[ String]] {
      case Some(a) => a must_== "a"
    },
    beLike[Option[String]] {
      case Some(b) => b must_== "b"
    }))
  }

  // custom matchers can be created by coercing a function T => (Boolean, String, String) to Matcher[T]
  def endWithWorld: Matcher[String] = (s: String) => (s.endsWith("world"), s+" ends with world", s+" doesn't end with world")
}