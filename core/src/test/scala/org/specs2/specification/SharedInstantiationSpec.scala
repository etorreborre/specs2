package user

import org.specs2.*
import org.specs2.concurrent.ExecutionEnv
import scala.concurrent.Future
import org.specs2.main.*

/**
 * the following specifications should be instantiable from both the JVM and ScalaJS
 */

class Shared1Spec extends mutable.Spec:
  import scala.concurrent.ExecutionContext.Implicits.global
  "This must work with the global Execution Context should" >> {
    "match a future using both jvm + scalajs" >>
      Future(ok)
  }

class Shared2Spec(using ee: ExecutionEnv) extends mutable.Specification:
  "This must work with an implicit execution env should" >>{
    "match a future using both jvm + scalajs" >>
      Future(ok)
  }

class Shared3Spec(arguments: Arguments) extends mutable.Specification:
  "this is ok" >> ok
