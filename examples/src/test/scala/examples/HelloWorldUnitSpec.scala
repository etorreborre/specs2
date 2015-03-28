package examples

import java.util.concurrent.ExecutorService

import org.specs2._
import org.specs2.specification.core.Env

import scala.concurrent.ExecutionContext

class HelloWorldUnitSpec extends mutable.Specification {
  "The 'Hello world' string" should {
    "contain 11 characters" in {
      "Hello world" must haveSize(11)
    }
    "start with 'Hello'" in {
      "Hello world" must startWith("Hello")
    }
    "end with 'world'" in {
      "Hello world" must endWith("world")
    }
    "end with 'world'" >> { env: Env =>
      "Hello world" must endWith("world")
    }
    "end with 'world'" >> { ec: ExecutorService =>
      "Hello world" must endWith("world")
    }
    "end with 'world'" >> { ec: ExecutionContext =>
      "Hello world" must endWith("world")
    }
  }
}
