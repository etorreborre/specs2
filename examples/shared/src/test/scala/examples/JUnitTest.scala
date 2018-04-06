package examples
import org.junit._
import org.specs2.matcher._

/**
 * This class shows how to use specs2 matchers in a JUnit specification
 */
class JUnitTest extends JUnitMustMatchers {

  @Test
  def test1(): Unit = {
    "Hello world" must haveSize(11); ()
  }

  @Test
  def test2(): Unit = {
    "Hello world" must startWith("Hello"); ()
  }

  @Test
  def test3(): Unit = {
    "Hello world" must endWith("world"); ()
  }

  @Test
  def test4(): Unit = {
    "Hello world" must endWithWorld; ()
  }

  // custom matchers can be created by coercing a function T => (Boolean, String, String) to Matcher[T]
  def endWithWorld: Matcher[String] = { s: String =>
    (s.endsWith("world"), s+" ends with world", s+" doesn't end with world")
  }
}
