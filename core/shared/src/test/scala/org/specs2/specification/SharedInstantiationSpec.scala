package user

import org.specs2._
import org.specs2.concurrent.ExecutionEnv
import scala.concurrent.Future
import org.specs2.main._

/**
 * the following specifications should be instantiable from both the JVM and ScalaJS
 */

@annotation.nowarn
class Shared1Spec extends mutable.Specification {
  import scala.concurrent.ExecutionContext.Implicits.global
  "This must work with the global Execution Context" should {
    "match a future using both jvm + scalajs" in {
      Future(ok)
    }
  }
}

class Shared2Spec(implicit ee: ExecutionEnv) extends mutable.Specification {
  "This must work with an implicit execution env" should {
    "match a future using both jvm + scalajs" in {
      Future(ok)
    }
  }
}

class Shared3Spec(arguments: Arguments) extends mutable.Specification {
  "this is ok" >> ok
}
