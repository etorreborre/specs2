package user.reporter

import org.specs2._

class TestSpec extends mutable.Specification {
  //args.execute(asap = true)

  (1 to 100).foreach { i =>
    "test"+i >> {
      Thread.sleep(scala.math.round(scala.math.random * 100))
      ("executed "+i).pp
      ok
    }
  }
}
