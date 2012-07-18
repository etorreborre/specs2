package examples

import org.specs2._

class HelloWorldUnitSpec extends mutable.Specification {
(1 to 10) foreach { identity =>
  "The 'Hello world' string" should {
    "contain 11 characters" in {
      Thread.sleep(50)
      println("1")
      "Hello world" must have size(11)
    }
    "start with 'Hello'" in {
      Thread.sleep(50)
      println("2")
      "Hello world" must startWith("Hello")
    }
    "end with 'world'" in {
      Thread.sleep(50)
      println("3")

      "Hello world" must endWith("world")
    }
  }
}
}
